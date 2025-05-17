import json
import os
from typing import Optional

import chainlit as cl
from chainlit import user_session
from openai import AsyncAssistantEventHandler, AsyncOpenAI, OpenAI
from openai.types.beta import AssistantStreamEvent
from openai.types.beta.threads import Text, TextDelta
from typing_extensions import override

from function_tools import (
    generate_image,
    getHistoricalCountriesByCurrentCountryAndYear,
    getHistoricalEvents,
    getMilitaryConflicts,
    getWarsByHistoricalCountryAndYear,
    imageSearch,
    mapCombined,
    mapHistoricalContinent,
    mapHistoricalCountry,
    mapWar,
    timelineCombined,
    timelineHistoricalPeriodsAndNotablePeople,
    timelinePerson,
    web_search_brave,
)
from license_management import (
    QuotaExceededException,
    get_license_key,
    upsert_license_key,
    verify_license,
)

async_openai_client = AsyncOpenAI(api_key=os.environ["OPENAI_API_KEY"])
openai_client = OpenAI(api_key=os.environ["OPENAI_API_KEY"])

assistant = openai_client.beta.assistants.retrieve(
    assistant_id = os.environ['OPENAI_ASSISTANT_ID']
)
print(f"Retrieved Assistant: {assistant.name} ID: {assistant.id}")

with open('LICENSE_KEY_HOW_TO.md', 'r') as file:
    LICENSE_KEY_HOW_TO = file.read()
with open('WELCOME_FREE_TRIAL.md', 'r') as file:
    WELCOME_FREE_TRIAL = file.read()


class MessageEventHandler(AsyncAssistantEventHandler):
    def __init__(self) -> None:
        super().__init__()
        self.current_message: Optional[cl.Message] = None

    @override
    async def on_text_created(self, text: Text) -> None:
        self.current_message = cl.Message(author="AI Tutor History", content="")
        await self.current_message.send()

    @override
    async def on_text_delta(self, delta: TextDelta, snapshot: Text) -> None:
        assert self.current_message is not None, "current_message should be set before on_text_delta"
        await self.current_message.stream_token(delta.value or "")

    @override
    async def on_text_done(self, text: Text) -> None:
        assert self.current_message is not None, "current_message should be set before on_text_done"
        # need this to sopt the dot from blinking
        await self.current_message.update()

    @override
    async def on_event(self, event: AssistantStreamEvent) -> None:
        # Retrieve events that are denoted with 'requires_action'
        # since these will have our tool_calls
        if event.event == 'thread.run.requires_action':
            try:
                await self.handle_requires_action(event.data)
            # catch QuotaExceededException
            except QuotaExceededException:
                await cl.Message(
                    author="AI Tutor History", 
                    content="⚠️ Your visualization credits have been used up! To continue exploring history, please purchase a new license key 🎟️. Get ready for more learning adventures! 📜✨"
                ).send()
                license_key = await get_license_key_from_user()
                cl.user_session.set("license_key", license_key)
                await self.handle_requires_action(event.data)
            

    async def handle_requires_action(self, data):
        tool_outputs = []
        for tool in data.required_action.submit_tool_outputs.tool_calls:
            arguments = json.loads(tool.function.arguments)
            try:
                if tool.function.name == "getHistoricalCountriesByCurrentCountryAndYear":
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await getHistoricalCountriesByCurrentCountryAndYear(
                                currentCountry= arguments["currentCountry"], 
                                year = arguments["year"]
                            )
                        )
                    })
                elif tool.function.name == "getHistoricalEvents":
                    await task_running("context")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await getHistoricalEvents(
                                year = arguments["year"],
                                yearsBack = arguments.get("yearsBack", None)
                            )
                        )
                    })
                    await task_done("context")
                elif tool.function.name == "getMilitaryConflicts":
                    await task_running("context")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(                                           
                            await getMilitaryConflicts(year = arguments["year"])
                        )
                    })
                    await task_done("context")
                elif tool.function.name == "timelineHistoricalPeriodsAndNotablePeople":
                    await task_running("context")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await timelineHistoricalPeriodsAndNotablePeople(year = arguments["year"])
                        )
                    })
                    await task_done("context")
                elif tool.function.name == "getWarsByHistoricalCountryAndYear":
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await getWarsByHistoricalCountryAndYear(
                                historicalCountry= arguments["historicalCountry"], 
                                year = arguments["year"]
                            )
                        )
                    })
                elif tool.function.name == "mapHistoricalContinent":
                    await task_running("map")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await mapHistoricalContinent(
                                year = arguments["year"],
                                continent = arguments["continent"]
                            )
                        )
                    })
                    await task_done("map")
                elif tool.function.name == "mapHistoricalCountry":
                    await task_running("map")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await mapHistoricalCountry(
                                historicalCountry=arguments["historicalCountry"],
                                year = arguments["year"],
                                geoRange=arguments.get("geoRange", None)
                            )
                        )
                    })
                    await task_done("map")
                elif tool.function.name == "imageSearch":
                    await task_running("visual")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await imageSearch(searchString=arguments["searchString"])
                        )
                    })
                    await task_done("visual")
                elif tool.function.name == "timelinePerson":
                    await task_running("timeline")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await timelinePerson(person=arguments["person"])
                        )
                    })
                    await task_done("timeline")
                elif tool.function.name == "mapWar":
                    await task_running("map")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await mapWar(
                                militaryConflict=arguments["militaryConflict"],
                                geoRange=arguments.get("geoRange", None)
                            )
                        )
                    })
                    await task_done("map")
                elif tool.function.name == "generate_image":
                    await task_running("visual")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await generate_image(prompt=arguments["prompt"])
                        )
                    })
                    await task_done("visual")
                elif tool.function.name == "timelineCombined":
                    await task_running("timeline")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await timelineCombined(
                                periods=arguments.get("periods", None),
                                historicalCountries=arguments.get("historicalCountries", None),
                                militaryConflicts=arguments.get("militaryConflicts", None),
                                persons=arguments.get("persons", None))
                        )
                    })
                    await task_done("timeline")
                elif tool.function.name == "mapCombined":
                    await task_running("map")
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(
                            await mapCombined(
                                historicalCountries=arguments.get("historicalCountries", None),
                                yearForHistoricalCountries=arguments.get("yearForHistoricalCountries", None),
                                militaryConflicts=arguments.get("militaryConflicts", None),
                                countries=arguments.get("countries", None),
                                cities=arguments.get("cities", None),
                                periods=arguments.get("periods", None))
                        )
                    })
                    await task_done("map")
                elif tool.function.name == "web_search_brave":
                    await task_running("search")
                    if "freshness" in arguments:
                        output = await web_search_brave(
                            arguments["q"], 
                            freshness=arguments["freshness"])
                    else:
                        output = await web_search_brave(arguments["q"])
                    tool_outputs.append({
                        "tool_call_id": tool.id,
                        "output": json.dumps(output)
                    })
                    await task_done("search")
                else:
                    # Assert that unexpected tool function is never reached
                    raise AssertionError(
                        f"Unexpected tool function called: {tool.function.name}")
            except KeyError as e:
                tool_outputs.append({
                    "tool_call_id": tool.id,
                    "output": f"Error: {e} not found in arguments."
                })
            except AssertionError:
                tool_outputs.append({
                    "tool_call_id": tool.id,
                    "output": f"Error: Unknown tool: {tool.function.name}."
                })
        await self.submit_tool_outputs(tool_outputs)
        
    async def submit_tool_outputs(self, tool_outputs):
        # Use the submit_tool_outputs_stream helper
        assert self.current_run is not None, "self.current_run should be set before calling submit_tool_outputs"
        thread_id = user_session.get("thread_id")
        assert thread_id is not None, "thread_id should be set before calling submit_tool_outputs"
        async with async_openai_client.beta.threads.runs.submit_tool_outputs_stream(
                thread_id=self.current_run.thread_id,
                run_id=self.current_run.id,
                tool_outputs=tool_outputs,
                event_handler=MessageEventHandler(),
        ) as stream:
            await stream.until_done()


async def get_assistant_message() -> None:
    thread_id = cl.user_session.get("thread_id")
    assert thread_id is not None, "No thread_id found in user_session"
    async with async_openai_client.beta.threads.runs.stream(
          thread_id=thread_id,
          assistant_id=assistant.id,
          event_handler=MessageEventHandler()
        ) as stream:
          try:
            # why do we need this? Becasue that is how we can cancel a run
            async for event in stream:
              if event.event == "thread.run.created":
                run_id = event.data.id
                cl.user_session.set("run_id", run_id)
                break
          finally:
            await stream.until_done()
            run_id = None
            cl.user_session.set("run_id", run_id)

async def get_license_key_from_user() -> str:
    """
    Get the license key from the user. Keep asking until a valid key is provided.
    """
    license_key = None
    user = cl.user_session.get("user")
    assert user is not None, "No user found in user_session"
    while not license_key:
        res = await cl.AskUserMessage(content=LICENSE_KEY_HOW_TO, timeout=300).send()
        license_key = res.get('output') if res else None
        if license_key:
            is_valid, use, max_use = await verify_license(license_key, increment_usage=False)
            if not is_valid:
                license_key = None
                await cl.Message(author="AI Tutor History", content="❌ Oops! That license key is invalid. Please double-check and try again 🚫.").send()
            else:
                check_db= await upsert_license_key(license_key, user.identifier)
                assert check_db is not None, "Failed to upsert license key"
                await cl.Message(author="AI Tutor History", content="🎉🙌 Thank you for purchasing a license key! You're all set to dive into history — enjoy using your new credits! 📜✨🎊").send()
    return license_key


@cl.on_chat_start
async def start():
    # get authenticated user
    user = cl.user_session.get("user")
    assert user is not None, "No user found in user_session"
    # make sure there is a valid license key
    license_key = await get_license_key(user.identifier)
    if not license_key:
        cl.user_session.set("trial_counter", 0)
        actions = [
            cl.Action(
                name='enter_license_key', 
                value='enter_license_key', 
                label='Enter License Key', 
                description='When you have purchased a license key in our store you can enter it here.'
            )
        ]
        await cl.Message(
            author="AI Tutor History", 
            content=WELCOME_FREE_TRIAL, 
            actions=actions
        ).send()
    else:
        cl.user_session.set("license_key", license_key)
    # create thread
    thread = await async_openai_client.beta.threads.create()
    cl.user_session.set("thread_id", thread.id)
    # show the task list
    await show_task_list()
    # AI starts
    await get_assistant_message()

@cl.action_callback('enter_license_key')
async def on_enter_license_key(action: cl.Action):
    license_key = await get_license_key_from_user()
    cl.user_session.set("license_key", license_key)
    await action.remove()   

@cl.on_stop
async def stop():
    run_id = cl.user_session.get("run_id")
    thread_id = cl.user_session.get("thread_id")
    assert thread_id is not None, "No thread_id found in user_session"
    if run_id is not None:
        try:
            await async_openai_client.beta.threads.runs.cancel(
                thread_id=thread_id,
                run_id=run_id)
        except Exception as e:
            print(f"Error cancelling run: {str(e)}")

@cl.on_chat_end
async def end():
    user = cl.user_session.get("user")
    if user:
        print(f"on_chat_end for user {user}")
    else:
        print("on_chat_end but no user found in user_session")
    # thread_id = cl.user_session.get("thread_id")
    # if thread_id:
    #     await async_openai_client.beta.threads.delete(thread_id=thread_id)
    # cl.user_session.set("thread_id", None)
  
@cl.on_message
async def main(message: cl.Message) -> None:
    # submit the user's message to the assistant
    thread_id = cl.user_session.get("thread_id")
    assert thread_id is not None, "No thread_id found in user_session"
    await async_openai_client.beta.threads.messages.create(
      thread_id=thread_id,
      role = "user",
      content = message.content
    )
    # gen the assistant's response
    await get_assistant_message()

@cl.oauth_callback
def oauth_callback(
  provider_id: str,
  token: str,
  raw_user_data: dict[str, str],
  default_user: cl.User,
) -> Optional[cl.User]:
    _ = provider_id
    _ = token
    _ = raw_user_data
    return default_user


if __name__ == "__main__":
  from chainlit.cli import run_chainlit
  run_chainlit(__file__)

# Tasklist stuff

async def show_task_list() -> None:
    task_list = cl.TaskList()
    tasks = {
        "context": cl.Task(title="Get a global context of the cultural and geoplitical landscape."),
        "map": cl.Task(title="Look at it on the map."),
        "timeline": cl.Task(title="See it on a timeline."),
        "visual": cl.Task(title="Visualize it with illustrations or image collage."),
        "search": cl.Task(title="Search the web for legends, anecdotes, fun facts."),
    }
    for task_name, task in tasks.items():
        await task_list.add_task(task)
    cl.user_session.set("task_list", task_list)
    cl.user_session.set("tasks", tasks)
    await task_list.send()

async def task_done(task_desc: str) -> None:
    task_list = cl.user_session.get("task_list")
    if task_list is None:
        return
    tasks = cl.user_session.get("tasks")
    if tasks is None:
        return
    task = tasks[task_desc]
    task.status = cl.TaskStatus.DONE
    await task_list.update()

async def task_running(task_desc: str) -> None:
    task_list = cl.user_session.get("task_list")
    if task_list is None:
        return
    tasks = cl.user_session.get("tasks")
    if tasks is None:
        return
    task = tasks[task_desc]
    task.status = cl.TaskStatus.RUNNING
    await task_list.update()
