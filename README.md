# URL Shortener
This projects implements a simple URL Shortener in Haskell using AWS Lambda.
It provides:
1. A handler to store URLs together with its short form
2. A handler to fetch a URL from its short form. The result is returned as an HTTP redirect

The URLs are stored in a DynamoDB table
The handler to store URLs is protected by Google ReCaptcha v3.
The ReCaptcha private key is stored using AWS SSM. Regrettably, I could not use the new AWS Secrets Manager as it was more expensive.

[Here](https://s.mirko.io) you can find a deployed version of the project.
Please contact me if you need more details on how to build and deploy your URL shortener.
