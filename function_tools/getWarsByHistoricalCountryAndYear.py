import chainlit as cl

from .callWolframCloudAPI import callWolframCloudAPI


@cl.step(type="tool", name="getWarsByHistoricalCountryAndYear")
async def getWarsByHistoricalCountryAndYear(historicalCountry: str, year: str):
  endpoint = "/getWarsByHistoricalCountryAndYear"
  return await callWolframCloudAPI(
    endpoint = endpoint, 
    data = {"historicalCountry": historicalCountry, "year": year}
  )