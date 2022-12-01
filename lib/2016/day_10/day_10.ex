defmodule AdventOfCode.Y2016.Day10 do
  @moduledoc """
  --- Day 10: Balance Bots ---
  Problem Link: https://adventofcode.com/2016/day/10
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias __MODULE__.{Bot, BotSupervisor, ControlPanel, OutputBin}

  @input InputReader.read_from_file(2016, 10)

  @param [17, 61]
  def run_1 do
    BotSupervisor
    |> Supervisor.which_children()
    |> Enum.map(fn {_, pid, _, _} -> Bot.vals(pid) end)
    |> Enum.find(fn
      {_, @param} -> true
      _ -> false
    end)
    |> elem(0)
  end

  @param ~w/0 1 2/
  def run_2 do
    OutputBin
    |> :sys.get_state()
    |> Map.take(@param)
    |> Map.values()
    |> Enum.product()
  end

  def run(input \\ @input) do
    ControlPanel.start_servers()

    input
    |> parse()
    |> ControlPanel.message_bots()

    {run_1(), run_2()}
  end

  def parse(data) do
    lines = Transformers.lines(data)
    configurations = lines |> Enum.map(&parse_configuration/1) |> Enum.reject(&is_nil/1)
    assignments = lines |> Enum.map(&parse_assignment/1) |> Enum.reject(&is_nil/1)

    configurations
    |> Kernel.++(assignments)
    |> tap(fn instructions ->
      instructions
      |> parse_names()
      |> ControlPanel.create_bots()
    end)
  end

  @configuration ~r"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)"
  defp parse_configuration(line) do
    @configuration
    |> Regex.run(line, capture: :all_but_first)
    |> then(fn
      nil -> nil
      [bot, lo_sink, lo, hi_sink, hi] -> {bot, %{low: {lo_sink, lo}, high: {hi_sink, hi}}}
    end)
  end

  @assignment ~r"value (\d+) goes to bot (\d+)"
  defp parse_assignment(line) do
    @assignment
    |> Regex.run(line, capture: :all_but_first)
    |> then(fn
      nil -> nil
      [val, bot] -> {bot, String.to_integer(val)}
    end)
  end

  def parse_names(lines) do
    Enum.flat_map(lines, fn
      {id_1, %{high: {sink_2, id_2}, low: {sink_3, id_3}}} ->
        [id_1, (sink_2 === "bot" && id_2) || nil, (sink_3 && id_3) || nil]

      {id, _} ->
        [id]
    end)
  end
end
