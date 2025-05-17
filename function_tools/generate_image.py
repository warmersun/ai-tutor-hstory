import json
import os

import chainlit as cl
from openai import AsyncOpenAI

from license_management import increment_license_key_usage_counter


@increment_license_key_usage_counter
@cl.step(type="tool", name="generate_image")
async def generate_image(prompt: str):
    async_client = AsyncOpenAI(api_key=os.environ["OPENAI_API_KEY"])
    response = await async_client.images.generate(
        model="dall-e-3",
        prompt=prompt,
        size="1024x1024",
        quality="standard",
        n=1
    )
    image_url = response.data[0].url
    return json.dumps({"image": image_url})