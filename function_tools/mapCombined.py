from typing import Optional

import chainlit as cl

from license_management import increment_license_key_usage_counter

from .callWolframCloudAPI import callWolframCloudAPI


@increment_license_key_usage_counter
@cl.step(type="tool", name="mapCombined")
async def mapCombined(
  historicalCountries: Optional[str], 
  yearForHistoricalCountries: Optional[str], 
  militaryConflicts: Optional[str], 
  countries: Optional[str], 
  cities: Optional[str], 
  periods: Optional[str]
):
  endpoint = "/mapCombined"
  data = {}
  if historicalCountries:
    data["historicalCountries"] = historicalCountries
  if yearForHistoricalCountries:
    data["yearForHistoricalCountries"] = yearForHistoricalCountries
  if militaryConflicts:
    data["militaryConflicts"] = militaryConflicts
  if countries:
    data["countries"] = countries
  if cities:
    data["cities"] = cities
  if periods:
    data["periods"] = periods
  return await callWolframCloudAPI(endpoint = endpoint, data = data)