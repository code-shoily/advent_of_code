defmodule AdventOfCode.Y2016.Day10 do
  @moduledoc """
  --- Day 10: Balance Bots ---
  Problem Link: https://adventofcode.com/2016/day/10
  Difficulty: m
  Tags: graph simulation dataflow
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2016, 10)

  @target [17, 61]
  @out_bins ["0", "1", "2"]

  def run(input \\ input()) do
    {rules, inventory} = parse(input)

    starting_bots =
      inventory
      |> Enum.filter(fn {_, chips} -> length(chips) == 2 end)
      |> Enum.map(fn {id, _} -> id end)

    {part1, outputs} = solve_simulation(inventory, rules, starting_bots)

    part2 =
      @out_bins
      |> Enum.map(&outputs[&1])
      |> Enum.product()

    {String.to_integer(part1), part2}
  end

  defp solve_simulation(inventory, rules, ready, outputs \\ %{}, part1 \\ nil) do
    case ready do
      [] ->
        {part1, outputs}

      [bot_id | tail] ->
        chips = Enum.sort(inventory[bot_id])
        [low_v, high_v] = chips
        {low_target, high_target} = rules[bot_id]

        new_part1 = if chips == @target, do: bot_id, else: part1

        {new_inv, new_out, low_ready} = give(low_v, low_target, inventory, outputs)
        {new_inv, new_out, high_ready} = give(high_v, high_target, new_inv, new_out)
        new_inv = Map.delete(new_inv, bot_id)

        new_ready = tail ++ low_ready ++ high_ready

        solve_simulation(new_inv, rules, new_ready, new_out, new_part1)
    end
  end

  defp give(val, {type, id}, inventory, outputs) do
    if type == "output" do
      {inventory, Map.put(outputs, id, val), []}
    else
      existing = inventory[id] || []
      new_chips = [val | existing]
      new_inventory = Map.put(inventory, id, new_chips)
      ready = if length(new_chips) == 2, do: [id], else: []
      {new_inventory, outputs, ready}
    end
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.reduce({%{}, %{}}, fn line, {rules, inventory} ->
      cond do
        line =~ "gives low to" ->
          [bot, low_type, low_id, high_type, high_id] =
            Regex.run(
              ~r/bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)/,
              line,
              capture: :all_but_first
            )

          {Map.put(rules, bot, {{low_type, low_id}, {high_type, high_id}}), inventory}

        line =~ "value" ->
          [val, bot] = Regex.run(~r/value (\d+) goes to bot (\d+)/, line, capture: :all_but_first)

          {rules,
           Map.update(inventory, bot, [String.to_integer(val)], &[String.to_integer(val) | &1])}
      end
    end)
  end
end
