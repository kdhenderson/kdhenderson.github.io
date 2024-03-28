---
layout: page
title: Password Guardian
description: A Python Tool to Securely Verify Password Integrity
img: assets/img/lockedGateEdit.jpeg
importance: 4
category: past
---

<style>
    /* Custom CSS for formatting */
    .larger-font {
        font-size: 1.2em;
        line-height: 1.5; /* Increase line height for better readability */
        font-family: Arial, Helvetica, sans-serif; /* Change font family */
    }
    
    .paragraph {
        margin-bottom: 10px; /* Add margin bottom for paragraphs */
        font-family: Arial, Helvetica, sans-serif; /* Change font family */
    }
</style>

<div class="row">
    <div class="col-12 mt-3 mt-md-0">
        <h5 class="larger-font paragraph">This tool was developed to enhance password security by checking whether a password has been compromised in a data breach.</h5>

        <p class="paragraph">I built a tool using Python to check if a password has ever been leaked during a data breach using <abbr title="Have I Been Pwned">haveibeenpwned</abbr> without sending the full password to the API. <em>Have I Been Pwned</em> is a website that allows users to check whether their personal data has been compromised by data breaches.</p>

        <h5 class="larger-font paragraph">This is what the program does step-by-step:</h5>

        <p class="paragraph">
            - The user enters a password(s) through the command line using the sys library.
            <br>
            - A hashed password is generated with a SHA-1 hashing function using the hashlib library.
            <br>
            - The first 5 characters of the hashed password are sent to the haveibeenpwned API using the requests library, and the API responds with leaked passwords from its database that match the starting hash.
            <br>
            - On the local machine, the program checks the response data for a match to the rest of the hashed password and returns if a match exists and how many times the password has been pwned.
        </p>

        <h5 class="larger-font paragraph">Get the script and requirements.txt at my GitHub repository:</h5>
        <a href="https://github.com/kdhenderson/password_checker" title="" class="btn btn-default">GitHub Repository</a>

        <br> <!-- One line of spacing added after the button -->
        <br> <!-- One line of spacing added after the button -->
    </div>
</div>
