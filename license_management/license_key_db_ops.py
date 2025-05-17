import os
from typing import Optional

import asyncpg


async def get_license_key(user_identifier: str) -> Optional[str]:
  """Get the license key for a user."""
  conn = await asyncpg.connect(os.environ["DATABASE_URL"])
  try:
    result = await conn.fetchrow(
        "SELECT license_key FROM users WHERE username = $1",
        user_identifier)
    return result["license_key"] if result else None
  finally:
    await conn.close()

async def upsert_license_key(license_key: str, user_identifier: str) -> bool:
    """UPSERT license key for a user."""
    conn = await asyncpg.connect(os.environ["DATABASE_URL"])
    try:
        await conn.execute(
            """
            INSERT INTO users (username, license_key) 
            VALUES ($1, $2)
            ON CONFLICT (username)
            DO UPDATE SET license_key = EXCLUDED.license_key
            """, user_identifier, license_key)
        return True
    except Exception as e:
        print(f"Error upserting license key: {str(e)}")
        return False
    finally:
        await conn.close()


