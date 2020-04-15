#!/usr/bin/env python

# import json
# import subprocess

# def secure_string_for(account, service, value):
#     # this relies on the macOS `security` tool
#     # https://developer.apple.com/legacy/library/documentation/Darwin/Reference/ManPages/man1/security.1.html
#     return json.loads(subprocess.check_output(["security",
#                                                "find-generic-password",
#                                                "-a", account,
#                                                "-s", service,
#                                                "-w"]).strip())[value]
# security find-generic-password -a jvillasantegomez@gmail.com -s imap.gmail.com -g 

import re, subprocess
def get_keychain_pass(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-generic-password',
        'account': account,
        'server': server,
        'keychain': '/Users/`whoami`/Library/Keychains/login.keychain',
    }
    command = "sudo -u `whoami` %(security)s -v %(command)s -g -a %(account)s -s %(server)s %(keychain)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = [l for l in output.splitlines()
               if l.startswith('password: ')][0]

    return re.match(r'password: "(.*)"', outtext).group(1)
