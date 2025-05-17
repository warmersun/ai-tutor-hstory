import os

import chainlit as cl

from .verify_license import verify_license


async def show_license_usage(uses: int, max_uses: int) -> None:
    task_list = cl.user_session.get("task_list")
    if task_list is None:
        return
    task_list.status = f"License usage: {uses}/{max_uses}"
    await task_list.update()

async def show_trial_usage(uses: int) -> None:
    task_list = cl.user_session.get("task_list")
    if task_list is None:
        return
    max_trial_usage = int(os.environ.get('MAX_TRIAL_USAGE', '3'))
    task_list.status = f"Trial usage: {uses}/{max_trial_usage}"
    await task_list.update()


class QuotaExceededException(Exception):

    def __init__(
        self,
        message="The quota associated with the user's license key has been reached."
    ):
        self.message = message
        super().__init__(self.message)


def increment_license_key_usage_counter(func):

    async def wrapper(*args, **kwargs):
        license_key = cl.user_session.get('license_key')
        # if there is no license key then it is a trial user
        if license_key:
            # licensed user
            verified, use, max_use = await verify_license(
                license_key=license_key)
            if verified:
                await show_license_usage(use, max_use)
                return await func(*args, **kwargs)
            else:
                raise QuotaExceededException()
        else:
            # trial user
            trial_counter = cl.user_session.get('trial_counter')
            assert trial_counter is not None, "No trial counter found in user_session"
            trial_counter += 1
            cl.user_session.set("trial_counter", trial_counter)
            if trial_counter <= int(os.environ.get('MAX_TRIAL_USAGE', '3')):
                await show_trial_usage(trial_counter)
                return await func(*args, **kwargs)
            else:
                raise QuotaExceededException()
    return wrapper


# example usage of this decorator:
# @increment_license_key_usage_counter
# async def my_function():
#     # code that requires a valid license key
