#! /usr/bin/env python3
# -*- coding: UTF-8 -*-

import argparse
import flickrapi
import json
import os
import os.path
import urllib

from collections    import namedtuple

# Fetches public images using the Flickr API.

# Which picture to download. Medium 640 is max. 640 x 640 pixels.
SOURCE_SIZE = "Medium 640"

Photo = namedtuple("Photo", "id title owner tags url")
Owner = namedtuple("Owner", "id username name")

def photos_stream(ids, flickr, license):
    """
    Returns an iteraror of Photo objects until the Flickr search is exhausted.
    """

    page = 1
    prev_ids = set()
    while True:
        print ("Asking for page %d ..." % page)

        photos = flickr.photos.search(
            page=page, sort="relevance", license=license # CC Attribution License
        )
        if is_flickr_error(photos):
            print("Unable to fetch the search results. Retrying ...")
            continue

        this_ids = set(p["id"] for p in photos["photos"]["photo"])
        if this_ids == prev_ids:
            print("Flickr gave the same results as the previous request. Stop.")
            return
        prev_ids = this_ids

        for p in photos["photos"]["photo"]:
            # Fetches meta-data and source image for each result.
            if p["id"] in ids:
                print("File %s already exists. Skip." % p["id"])
                continue

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
            )
            if source == None:
                print(
                    "The picture (%s) has no corresponding image. Skip."
                    % p["id"]
                )
                continue
            source_url = source["source"]
            if source_url[-4:] != ".jpg" and source_url[-5:] != ".jpeg":
                print("The picture (%s) is not a JPEG image. Skip." % p["id"])
                continue

            try:
                jpg = download_url(source_url)
            except:
                print("Failed to download the picture (%s). Skip." % p["id"])
                continue

            owner = Owner(
                p["owner"], info["photo"]["owner"]["path_alias"],
                info["photo"]["owner"]["realname"]
            )

            tags = [ t["raw"].lower()
                     for t in info["photo"]["tags"]["tag"]
                     if not t["machine_tag"] and is_valid_tag(t["raw"]) ]

            url = "https://www.flickr.com/photos/{0}/{1}/".format(
                owner.id, p["id"]
            )

            yield (Photo(p["id"], p["title"], owner, tags, url), jpg)

            ids.add(p["id"])

        page += 1
        if page >= photos["photos"]["pages"]:
            return

def is_flickr_error(resp):
    return resp["stat"] != "ok"

def is_valid_tag(tag):
    def is_ascii_alpha_num(c):
        c_ord = ord(c)
        return ord('A') <= c_ord <= ord('Z') or \
               ord('a') <= c_ord <= ord('z') or \
               ord('0') <= c_ord <= ord('9')

    return all(is_ascii_alpha_num(c) for c in tag)

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

    parser.add_argument(
        "--license", metavar="license", type=int, default=4,
        help="""License of downloaded pictures (0: All Rights Reserved,
             1: Attribution-NonCommercial-ShareAlike License,
             2: Attribution-NonCommercial License,
             3: Attribution-NonCommercial-NoDerivs License,
             4: Attribution License,
             5: Attribution-ShareAlike License,
             6: Attribution-NoDerivs License,
             7: No known copyright restrictions,
             8: United States Government Work)
             """
    )

    return parser

def find(pred, xs):
    for x in xs:
        if pred(x):
            return x

    return None

def existing_files(dest_dir):
    """Returns the set of IDs of existing files."""
    return set(os.path.splitext(f)[0] for f in os.listdir(dest_dir)
               if os.path.isfile(os.path.join(dest_dir, f)))

if __name__ == "__main__":
    cli_args   = get_cli_args_parser().parse_args()
    api_key    = cli_args.api_key
    api_secret = cli_args.api_secret
    dest_dir   = cli_args.dest_dir
    license    = cli_args.license

    flickr = flickrapi.FlickrAPI(api_key, api_secret, format='parsed-json')

    ids = existing_files(dest_dir)
    print ("%d pictures already downloaded." % len(ids))

    for (p, jpg) in photos_stream(ids, flickr, license):
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
