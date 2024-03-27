---
layout: page
title: Python Password Security Checker
description: A Tool to Check if Passwords Have Been Leaked
img: assets/img/lockedGate770_498.jpeg
importance: 4
category: past
---

<div class="row">
    <div class="col-12 mt-3 mt-md-0">

#### This is a tool I built to check if a password has ever been leaked during a data breach using haveibeenpwned without sending the full password to the API.

## This is what the program does step-by-step:

- The user enters a password(s) through the command line using the sys library.
- A hashed password is generated with a SHA-1 hashing function using the hashlib library.
- The first 5 characters of the hashed password are sent to the haveibeenpwned API using the requests library, and the API responds with leaked passwords from its database that match the starting hash.
- On the local machine, the program checks the response data for a match to the rest of the hashed password and returns if a match exists and how many times the password has been pwned.

## Check out the github repository:

<a href="https://github.com/kdhenderson/password_checker" title="" class="btn btn-default">GitHub Repository</a>

</div>

