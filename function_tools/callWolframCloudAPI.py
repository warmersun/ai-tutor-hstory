import asyncio
import json
import os
from datetime import datetime, timedelta, timezone

import chainlit as cl
import httpx
from replit import db

WOLFRAM_CLOUD_BASE_URL = "https://www.wolframcloud.com/obj/tamas.simon"

async def callWolframCloudAPI(endpoint: str, data: dict):
  # with a leading slash (/) the Replit key/value DB does not work!
  cache_key = f":{endpoint}:{json.dumps(data)}"
  # look in cache
  cache_value = db.get(cache_key, None)
  # not in cache
  if cache_value is None:
    data["_key"] = os.environ["WOLFRAM_CLOUD_API_PERMISSION_KEY"]
    async with httpx.AsyncClient() as wolfram_cloud_client:
      try:
          if endpoint.startswith(('/map', '/timeline')):
            # sync/async handling
            urlSync = WOLFRAM_CLOUD_BASE_URL + endpoint + "Sync"
            urlAsync = WOLFRAM_CLOUD_BASE_URL + endpoint + "Async"
            responseSync = await wolfram_cloud_client.post(urlSync, params=data, timeout=30)
            if responseSync.status_code == 200:
              # do the async call in the background, callback when it's done
              
              async def callback_async(future):
                responseAsync = future.result()
                if responseAsync.status_code == 200:
                  try:
                    containerAsync = responseAsync.json()
                    resultAsync = json.loads(containerAsync["Result"])
                    imageUrl = resultAsync.get("image")
                    if imageUrl:
                      # ignore the pyright warning here
                      msg2 = cl.Message(content=f"![{cache_key}]({imageUrl})")
                      await msg2.send()
                      # cache it! 
                      # Need to create a combined dict from the Sync and the Async response
                      containerSync = responseSync.json()
                      resultSync = json.loads(containerSync["Result"])
                      result_sync_and_async = resultSync
                      result_sync_and_async["image"] = resultAsync["image"]
                      if "assistantNotesAboutTheImage" in result_sync_and_async:
                        del result_sync_and_async["assistantNotesAboutTheImage"]
                      db[cache_key] = json.dumps({
                        "result": result_sync_and_async, 
                        "timestamp": datetime.now(timezone.utc).timestamp()
                      })
                    else:
                      print(f"No image found for {cache_key}")
                      msg5 = cl.Message(content="💩💣😢 Oops. Something went wrong: there is no image 🖼. Sorry! 😳")
                      await msg5.send()
                  except json.JSONDecodeError:
                    print(f"Error decoding JSON: {responseAsync.text}")
                    msg6 = cl.Message(content="💩💣😢 Oops. Something went wrong: Error Decoding JSON. Sorry! 😳")
                    await msg6.send()

                else:
                  print(f"Error calling {urlAsync}: status_code: {responseAsync.status_code}")
                  try:
                    containerAsync = responseAsync.json()
                    resultAsync = json.loads(containerAsync["Result"])
                    msg4 = cl.Message(content=f"💩💣😢 Oops. Something went wrong: `{resultAsync.get('Failure')}` Sorry! 😳")
                    await msg4.send()
                  except json.JSONDecodeError:
                    print(f"Error decoding JSON: {responseAsync.text}")
                    msg7 = cl.Message(content="💩💣😢 Oops. Something went wrong, Sorry! 😳")
                    await msg7.send()

              def callback(future):
                asyncio.create_task(callback_async(future))

              async def post_async_part(url: str, params: dict, timeout: int):
                async with httpx.AsyncClient() as wolfram_cloud_client_async_part:
                  try:
                    return await wolfram_cloud_client_async_part.post(url, params=params, timeout=timeout)
                  except httpx.TimeoutException:
                    print(f"Error: Timeout calling {url}")
                    msg3 = cl.Message(content="💩💣😢 Timeout ⏳ Sorry! 😳")
                    await msg3.send()
                    return httpx.Response(status_code=500, json={"error": "The async request timed out."})
              asyncio.create_task(
                post_async_part(url=urlAsync, params=data, timeout=30)
              ).add_done_callback(callback)

          else:
            url = WOLFRAM_CLOUD_BASE_URL + endpoint
            responseSync = await wolfram_cloud_client.get(url, params=data, timeout=30)
      except httpx.TimeoutException:
          await houseKeeping()
          return "The request timed out."
      if responseSync.status_code == 200:
        container = responseSync.json()
        result = json.loads(container["Result"])
        # cache it here, unless it's an image search or the sync/async case
        if endpoint!= "imageSearch" and not endpoint.startswith(('/map', '/timeline')):
          db[cache_key] = json.dumps({
            "result": result, 
            "timestamp": datetime.now(timezone.utc).timestamp()
          })
        # show a placeholder
        if endpoint.startswith(('/map', '/timeline')):
          result["image"] = "https://s3.amazonaws.com/ailcstatic.warmersun.com/visualbelow.gif"
        # ...and return
        return result
      elif responseSync.status_code == 400:
        container = responseSync.json()      
        assert "Result" in container, "Key 'Result' not found in the response."
        result_str = container["Result"]
        try:
          result = json.loads(result_str)
          if result.get("Failure", None) == "Request timed out" :
            # housekeeping in the Wolfram Cloud
            await houseKeeping()
        except json.JSONDecodeError:
          return f"Error parsing JSON: {result_str}"
        return result
      else:
        return f"Error: {responseSync.status_code}"
  else:
    # found in cache
    # deserialize it first
    cache_value_parsed = json.loads(cache_value)
    # expire cache if older than 1 week
    one_week_ago = (datetime.now(timezone.utc) - timedelta(days=7)).timestamp()
    timestamp = cache_value_parsed.get("timestamp", None)
    assert timestamp is not None, "timestamp is None"
    if timestamp < one_week_ago:
      del db[cache_key]
    # return the result
    result = cache_value_parsed.get("result", None)
    return result

async def houseKeeping():
  # housekeeping in the Wolfram Cloud
  url = WOLFRAM_CLOUD_BASE_URL + "/clearEntityValueCache"
  data = {"_key": os.environ["WOLFRAM_CLOUD_API_PERMISSION_KEY"]}
  await callWolframCloudAPI(endpoint=url, data=data)

def clear_cache():
  """clears the cache"""
  for cache_key in db.keys():
      del db[cache_key]

# main method calls clear_cache() in debug mode
if __name__ == "__main__":
  clear_cache()