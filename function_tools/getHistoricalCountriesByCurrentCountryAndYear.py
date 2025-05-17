import chainlit as cl

from .callWolframCloudAPI import callWolframCloudAPI


@cl.step(type="tool", name="getHistoricalCountriesByCurrentCountryAndYear")
async def getHistoricalCountriesByCurrentCountryAndYear(currentCountry: str, year: str):
  endpoint = "/getHistoricalCountriesByCurrentCountryAndYear"
  return await callWolframCloudAPI(endpoint = endpoint, data = {"currentCountry": currentCountry, "year": year})