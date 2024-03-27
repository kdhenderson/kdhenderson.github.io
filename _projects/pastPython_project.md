---
layout: page
title: Password Checker
description: A Python Tool to Check if Passwords Have Been Leaked
img: assets/img/lockedGateEdit.jpeg
importance: 4
category: past
---

<div class="row">
    <div class="col-12 mt-3 mt-md-0">

<h4>This is a tool to check if a password has ever been leaked during a data breach using haveibeenpwned without sending the full password to the API.</h4>
    
<h6>This is what the program does step-by-step:</h6>
<p>
- The user enters a password(s) through the command line using the sys library.
<br>
- A hashed password is generated with a SHA-1 hashing function using the hashlib library.
<br>
- The first 5 characters of the hashed password are sent to the haveibeenpwned API using the requests library, and the API responds with leaked passwords from its database that match the starting hash.
<br>
- On the local machine, the program checks the response data for a match to the rest of the hashed password and returns if a match exists and how many times the password has been pwned.
</p>

<h6>Check out the github repository:</h6>
<a href="https://github.com/kdhenderson/password_checker" title="" class="btn btn-default">GitHub Repository</a>

</div>

