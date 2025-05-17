import asyncio
from typing import Optional

import chainlit as cl
from openai.types import image

from license_management import increment_license_key_usage_counter

from .callWolframCloudAPI import callWolframCloudAPI


@increment_license_key_usage_counter
@cl.step(type="tool", name="mapWar")
async def mapWar(militaryConflict: str, geoRange: Optional[str] = None ):
  endpoint = "/mapWar"
  data =  {"militaryConflict": militaryConflict}
  if geoRange:
    data["geoRange"] = geoRange
  return await callWolframCloudAPI(endpoint = endpoint, data = data)