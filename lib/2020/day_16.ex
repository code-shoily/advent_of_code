defmodule AdventOfCode.Y2020.Day16 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/16
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 16

  def run_1 do
    {ranges, tickets} = process(input!())

    Enum.sum(
      Enum.filter(tickets, fn ticket ->
        ranges |> Enum.map(&(ticket not in &1)) |> Enum.all?()
      end)
    )
  end

  def run_2, do: {:not_implemented, 2}
  def process(input), do: {get_ranges(input), nearby_tickets(input)}

  def get_ranges(input) do
    Regex.scan(~r/\d+\-\d+/, input)
    |> Enum.flat_map(& &1)
    |> Enum.map(&as_range/1)
    |> Enum.sort()
    |> Enum.reduce([], fn
      x, [] ->
        [x]

      x, [h | rst] ->
        case merge_ranges(x, h) do
          [a, b] -> [a, b | rst]
          [a] -> [a | rst]
        end
    end)
  end

  def nearby_tickets(input) do
    ~r/.+nearby\stickets:\s(?<tickets>.+)/
    |> Regex.named_captures(String.replace(input, "\n", " "))
    |> Map.get("tickets")
    |> String.replace(" ", ",")
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def merge_ranges(a..b = r1, x..y = r2),
    do:
      (Range.disjoint?(r1, r2) &&
         [r1, r2]) || [((a < x && a) || x)..((b > y && b) || y)]

  defp as_range(range), do: rangify(Enum.map(String.split(range, "-"), &String.to_integer/1))
  defp rangify([a, b]), do: a..b
end
