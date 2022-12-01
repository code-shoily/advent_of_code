defmodule AdventOfCode.Y2015.Day09 do
  @moduledoc """
  --- Day 9: All in a Single Night ---
  Problem Link: https://adventofcode.com/2015/day/9
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 9)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def run_1(input), do: input |> travel_all(&Enum.min/1)
  def run_2(input), do: input |> travel_all(&Enum.max/1)

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn route ->
      ~r/(\w+) to (\w+) = (\d+)/
      |> Regex.run(route, capture: :all_but_first)
      |> then(&List.to_tuple/1)
    end)
    |> to_graph()
  end

  defp to_graph(data) do
    routes =
      data
      |> Enum.flat_map(fn {c1, c2, dist} ->
        [{{c1, c2}, String.to_integer(dist)}, {{c2, c1}, String.to_integer(dist)}]
      end)
      |> Map.new()

    cities = routes |> Map.keys() |> Enum.flat_map(&Tuple.to_list/1) |> MapSet.new()

    {cities, routes}
  end

  defp travel_all({cities, routes}, min_max_fn),
    do: min_max_fn.(Enum.map(cities, &travel(&1, routes, cities, 0, min_max_fn)))

  defp travel(city, routes, cities, distance, min_max_fn) do
    cities = MapSet.delete(cities, city)

    (Enum.empty?(cities) && distance) ||
      cities
      |> Enum.map(&travel(&1, routes, cities, distance + routes[{city, &1}], min_max_fn))
      |> min_max_fn.()
  end
end
