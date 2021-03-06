terraform {
  backend "s3" {
    bucket = "nl.hjdskes.shiba"
    key    = "tf/shiba/terraform.tfstate"
    region = "eu-west-1"
  }

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 3.0"
    }
  }
}

provider "aws" {
  region = "eu-west-1"
}

locals {
  zipfile = "../result/shiba-scraper.zip"
  tags = {
    ProjectID = "shiba"
  }
}

resource "aws_sns_sms_preferences" "sms_preferences" {
  monthly_spend_limit = 10
  delivery_status_success_sampling_rate = 100
  default_sender_id = "Shiba"
  default_sms_type = "Promotional"
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

resource "aws_iam_role_policy_attachment" "shiba_scraper_dynamodb" {
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
    resources = [ "arn:aws:logs:eu-west-1:*:*" ]
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

data "aws_iam_policy_document" "lambda_sns_document" {
  statement {
    actions   = [
      "sns:Publish"
    ]
    resources = [
      "*"
    ]
  }
}

resource "aws_iam_policy" "lambda_sns" {
  name        = "lambda_sns"
  description = "IAM policy for accessing SNS"
  path        = "/shiba/"
  policy      = data.aws_iam_policy_document.lambda_sns_document.json
}

resource "aws_iam_role_policy_attachment" "shiba_scraper_sns" {
  role       = aws_iam_role.shiba_scraper_iam_role.name
  policy_arn = aws_iam_policy.lambda_sns.arn
}

resource "aws_cloudwatch_event_rule" "hourly" {
  name                = "hourly"
  description         = "Fires every hour"
  schedule_expression = "rate(1 hour)"

  tags = local.tags
}

resource "aws_cloudwatch_event_target" "invoke_shiba_scraper_hourly" {
  rule      = aws_cloudwatch_event_rule.hourly.name
  target_id = "shiba_scraper"
  arn       = aws_lambda_function.shiba_scraper.arn
}

resource "aws_lambda_permission" "shiba_scraper_permission" {
  statement_id  = "AllowExecutionFromCloudWatch"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.shiba_scraper.function_name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.hourly.arn
}