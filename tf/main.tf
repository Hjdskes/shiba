terraform {
  backend "s3" {
    bucket = "nl.hjdskes.shiba"
    key    = "tf/shiba/terraform.tfstate"
    region = "eu-north-1"
  }

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 3.0"
    }
  }
}

provider "aws" {
  region = "eu-north-1"
}

locals {
  zipfile = "../result/shiba-scraper.zip"
  tags = {
    ProjectID = "shiba"
  }
}

resource "aws_dynamodb_table" "scraper_key_value_store" {
  name            = "scraper_key_value_store"
  billing_mode    = "PROVISIONED"
  write_capacity  = 25
  read_capacity   = 25
  hash_key        = "website"

  attribute {
    name = "website"
    type = "S"
  }

  tags = local.tags
}

resource "aws_lambda_function" "shiba_scraper" {
  function_name = "shiba_scraper"
  description   = "Shiba website scraper"

  handler          = "src/Scraper.handler"
  runtime          = "provided"
  role             = aws_iam_role.shiba_scraper_iam_role.arn
  filename         = local.zipfile
  source_code_hash = filebase64sha256(local.zipfile)
  timeout          = 10

  tags = local.tags
}

data "aws_iam_policy_document" "assume_role_policy" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "shiba_scraper_iam_role" {
  name               = "shiba_scraper_role"
  description        = "IAM role for the scraper Lambda"
  path               = "/shiba/"
  assume_role_policy = data.aws_iam_policy_document.assume_role_policy.json
}

data "aws_iam_policy_document" "lambda_dynamodb_document" {
  statement {
    actions   = [
      "dynamodb:GetItem",
      "dynamodb:PutItem",
      "dynamodb:UpdateItem"
    ]
    resources = [
      aws_dynamodb_table.scraper_key_value_store.arn
    ]
  }
}

resource "aws_iam_policy" "lambda_dynamodb" {
  name        = "lambda_dynamodb"
  description = "IAM policy for accessing DynamoDb"
  path        = "/shiba/"
  policy      = data.aws_iam_policy_document.lambda_dynamodb_document.json
}

resource "aws_iam_role_policy_attachment" "shiba_scraper_iam_role_policy_attachment" {
  role       = aws_iam_role.shiba_scraper_iam_role.name
  policy_arn = aws_iam_policy.lambda_dynamodb.arn
}

data "aws_iam_policy_document" "lambda_logging_document" {
  statement {
    actions = [
      "logs:CreateLogGroup",
      "logs:CreateLogStream",
      "logs:PutLogEvents"
    ]
    resources = [ "arn:aws:logs:eu-north-1:*:*" ]
  }
}

resource "aws_iam_policy" "lambda_logging" {
  name        = "lambda_logging"
  description = "IAM policy for logging from a lambda"
  path        = "/shiba/"
  policy      = data.aws_iam_policy_document.lambda_logging_document.json
}

resource "aws_iam_role_policy_attachment" "shiba_scraper_logs" {
  role       = aws_iam_role.shiba_scraper_iam_role.name
  policy_arn = aws_iam_policy.lambda_logging.arn
}