import chainlit as cl

from license_management import increment_license_key_usage_counter

from .callWolframCloudAPI import callWolframCloudAPI


@increment_license_key_usage_counter
@cl.step(type="tool", name="imageSearch")
async def imageSearch(searchString: str):
  endpoint = "/imageSearch"
  return await callWolframCloudAPI(
    endpoint = endpoint, 
    data = {"searchString": searchString}
  )