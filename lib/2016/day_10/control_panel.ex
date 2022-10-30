defmodule AdventOfCode.Y2016.Day10.ControlPanel do
  @moduledoc """
  Module to create and interact with bots.
  """
  alias AdventOfCode.Y2016.Day10.{Bot, BotSupervisor, OutputBin}

  @doc """
  Start all the servers. Both the output container and the dynamic supervisor
  """
  def start_servers do
    children = [
      %{
        id: OutputBin,
        start: {Agent, :start_link, [fn -> %{} end, [name: OutputBin]]}
      },
      {DynamicSupervisor, strategy: :one_for_one, name: BotSupervisor}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  @doc """
  Create all bots for all name in names.
  """
  def create_bots(names) do
    Enum.each(names, &DynamicSupervisor.start_child(BotSupervisor, {Bot, &1}))
  end

  @doc """
  Send a message to bots. Uses parsed instructions to send messages to related bots
  """
  def message_bots(instructions) do
    Enum.each(instructions, &message_bot/1)
  end

  defp message_bot({name, command}) do
    bot = :global.whereis_name(name)

    case command do
      value when is_integer(value) ->
        Bot.assign(bot, value)

      %{high: {sink_1, name_1}, low: {sink_2, name_2}} ->
        {hi_module, hi_pid} =
          (sink_1 == "bot" && {Bot, :global.whereis_name(name_1)}) || {OutputBin, name_1}

        {lo_module, lo_pid} =
          (sink_2 == "bot" && {Bot, :global.whereis_name(name_2)}) || {OutputBin, name_2}

        Bot.configure(bot, {lo_module, lo_pid}, {hi_module, hi_pid})
    end
  end
end
