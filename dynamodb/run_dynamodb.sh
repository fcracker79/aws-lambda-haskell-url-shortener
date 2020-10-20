#!/bin/sh

# docker run -p 8000:8000 amazon/dynamodb-local
docker run -v dynamodb_data:/home/dynamodblocal/data -p 8000:8000 amazon/dynamodb-local -jar DynamoDBLocal.jar -sharedDb -dbPath /home/dynamodblocal/data
