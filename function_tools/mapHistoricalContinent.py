import chainlit as cl

from license_management import increment_license_key_usage_counter

from .callWolframCloudAPI import callWolframCloudAPI


@increment_license_key_usage_counter
@cl.step(type="tool", name="mapHistoricalContinent")
async def mapHistoricalContinent(year: str, continent: str):
  endpoint = "/mapHistoricalContinent"
  return await callWolframCloudAPI(endpoint = endpoint, data = {"year": year, "continent": continent})