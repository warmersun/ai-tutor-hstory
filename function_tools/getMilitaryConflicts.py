import chainlit as cl

from .callWolframCloudAPI import callWolframCloudAPI


@cl.step(type="tool", name="getMilitaryConflicts")
async def getMilitaryConflicts(year: str):
  endpoint = "/getMilitaryConflicts"
  return await callWolframCloudAPI(endpoint = endpoint, data = {"year": year})