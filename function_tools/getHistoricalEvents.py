from typing import Optional

import chainlit as cl

from .callWolframCloudAPI import callWolframCloudAPI


@cl.step(type="tool", name="getHistoricalEvents")
async def getHistoricalEvents(year: str, yearsBack: Optional[int] = None):
  endpoint = "/getHistoricalEvents"
  data =  {"year": year}
  if yearsBack is not None:
    data["yearsBack"] = str(yearsBack)
  return await callWolframCloudAPI(endpoint = endpoint, data = data)