from typing import Optional

import chainlit as cl

from license_management import increment_license_key_usage_counter

from .callWolframCloudAPI import callWolframCloudAPI


@increment_license_key_usage_counter
@cl.step(type="tool", name="timelineCombined")
async def timelineCombined(
    periods: Optional[str] = None, 
    historicalCountries: Optional[str] = None, 
    militaryConflicts: Optional[str] = None, 
    persons: Optional[str] = None):
    endpoint = "/timelineCombined"
    data = {}
    if periods is not None:
        data['periods'] = periods
    if historicalCountries is not None:
        data['historicalCountries'] = historicalCountries
    if militaryConflicts is not None:
        data['militaryConflicts'] = militaryConflicts
    if persons is not None:
        data['persons'] = persons
    return await callWolframCloudAPI(endpoint=endpoint, data=data)