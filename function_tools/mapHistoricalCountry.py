from typing import Optional

import chainlit as cl

from license_management import increment_license_key_usage_counter

from .callWolframCloudAPI import callWolframCloudAPI


@increment_license_key_usage_counter
@cl.step(type="tool", name="mapHistoricalCountry")
async def mapHistoricalCountry(
  historicalCountry: str, 
  year: str, 
  geoRange: Optional[str] = None
):
  endpoint = "/mapHistoricalCountry"
  data =  {"historicalCountry": historicalCountry, "year": year}
  if geoRange:
    data["geoRange"] = geoRange
  return await callWolframCloudAPI(endpoint = endpoint, data = data)