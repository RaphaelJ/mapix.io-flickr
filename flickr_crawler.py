#! /usr/bin/env python3
# -*- coding: UTF-8 -*-

import argparse
import flickrapi
import json
import os.path
import urllib

from collections    import namedtuple

# Fetches public images using the Flickr API.

# Which picture to download. Medium 640 is max. 640 x 640 pixels.
SOURCE_SIZE = "Medium 640"

Photo = namedtuple("Photo", "id title owner tags url")
Owner = namedtuple("Owner", "id username name")

def photos_stream(flickr):
    """
    Returns an iteraror of Photo objects until the Flickr search is exhausted.
    """

    page = 1
    while True:
        photos = flickr.photos.search(
            page=page, sort="relevance", license="4" # CC Attribution License
        )
        if is_flickr_error(photos):
            print("Unable to fetch the search results. Retrying ...")
            continue

        for p in photos["photos"]["photo"]:
            # Fetches meta-data and source image for each result.

            info = flickr.photos.getInfo(photo_id=p["id"], secret=p["secret"])
            if is_flickr_error(info):
                print(
                    "Unable to fetch information for a picture (%s). Skip."
                    % p["id"]
                )
                continue

            sizes = flickr.photos.getSizes(photo_id=p["id"])
            if is_flickr_error(sizes):
                print(
                    "Unable to fetch information picture (%s) sources. Skip."
                    % p["id"]
                )
                continue

            source = find(
                lambda size: size["label"] == SOURCE_SIZE,
                sizes["sizes"]["size"]
            )["source"]
            if source == None:
                print(
                    "The picture (%s) has no corresponding image. Skip."
                    % p["id"]
                )
                continue
            if source[-4:] != ".jpg" and source[-5:] != ".jpeg":
                print("The picture (%s) is not a JPEG image. Skip." % p["id"])
                continue

            try:
                jpg = download_url(source)
            except:
                print("Failed to download the picture (%s). Skip." % p["id"])
                continue

            owner = Owner(
                p["owner"], info["photo"]["owner"]["path_alias"],
                info["photo"]["owner"]["realname"]
            )

            tags = [ t["raw"]
                     for t in info["photo"]["tags"]["tag"]
                     if not t["machine_tag"] ]

            url = "https://www.flickr.com/photos/{0}/{1}/".format(
                owner.id, p["id"]
            )

            yield (Photo(p["id"], p["title"], owner, tags, url), jpg)

        page += 1
        if page >= photos["photos"]["pages"]:
            return

def is_flickr_error(resp):
    return resp["stat"] != "ok"

def download_url(url):
    """Returns the file content."""
    return urllib.request.urlopen(url).read()

def write_file(filepath, content):
    """Writes the buffer content into the file."""

    if type(content) == str:
        flag = "w"
    elif type(content) == bytes:
        flag = "wb"

    with open(filepath, flag) as f:
        f.write(content)

def pretty_json(obj):
    """Renders the object as a pretty JSON string."""
    return json.dumps(obj, sort_keys=True, indent=4, separators=(',', ': '))

def get_cli_args_parser():
    parser = argparse.ArgumentParser(
        description="Fetches public images using the Flickr API.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    parser.add_argument(
        'api_key', metavar='api_key', type=str,
        help='Flickr API key'
    )

    parser.add_argument(
        'api_secret', metavar='api_key', type=str,
        help='Flickr API secret'
    )

    parser.add_argument(
        'dest_dir', metavar='dest_dir', type=str,
        help='Where the fetched images and meta-data should be stored'
    )

    return parser

def find(pred, xs):
    for x in xs:
        if pred(x):
            return x

    return None

if __name__ == "__main__":
    cli_args   = get_cli_args_parser().parse_args()
    api_key    = cli_args.api_key
    api_secret = cli_args.api_secret
    dest_dir   = cli_args.dest_dir

    flickr = flickrapi.FlickrAPI(api_key, api_secret, format='parsed-json')

    for (p, jpg) in photos_stream(flickr):
        basename  = os.path.join(dest_dir, p.id)
        jpg_file  = basename + ".jpg"
        json_file = basename + ".json"

        if os.path.isfile(jpg_file) or os.path.isfile(json_file):
            print("File %s already exists. Skip." % p.id)
            continue

        print("%s - %s" % (p.id, p.url))

        # Named-tuples are JSON encoded as array. Converts the two named-tuples
        # to dictionnaries before serializing.
        p_dict = p.__dict__
        p_dict["owner"] = p_dict["owner"].__dict__

        write_file(jpg_file, jpg)
        write_file(json_file, pretty_json(p_dict))
