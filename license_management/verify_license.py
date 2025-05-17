import json
import os

import httpx


async def verify_license(license_key: str, increment_usage=True) -> tuple[bool, int, int]:
  """Verify a license key and return the number of remaining uses.
     By default it will increment the quota counter"""
  data = {
      'product_id': os.environ['GUMROAD_PRODUCT_ID'],
      'license_key': license_key,
      'increment_uses_count': increment_usage
  }
  async with httpx.AsyncClient() as client:
      response = await client.post('https://api.gumroad.com/v2/licenses/verify', data=data)
      if response.status_code != 200:
          return False, 0, 0  # or any indication of failure and zero uses
      license_info = response.json()
      if os.environ.get('DEBUG_MODE') == '1':
          print(json.dumps(license_info, indent=4))
      success = license_info.get('success', False)
      uses = license_info.get('uses', 0)
      variant = license_info.get("purchase", {}).get("variants")
      max_uses = json.loads(os.environ['GUMROAD_MAX_USES'])
      print(f'Uses: {uses}')
      max_uses_for_variant = max_uses.get(variant, 0)
      return success and uses <= max_uses_for_variant, uses, max_uses_for_variant

def main():
    import asyncio
    license_key = os.environ['DEV_LICENSE_KEY']
    valid, uses, max_uses = asyncio.run(verify_license(license_key, increment_usage=False))
    print(f'License valid: {valid}, Usage counter: {uses}/{max_uses}')

if __name__ == '__main__' and os.environ.get('DEBUG_MODE') == '1':
    main()