defmodule AdventOfCode.Y2023.Day18 do
  @moduledoc """
  --- Day 18: Lavaduct Lagoon ---
  Problem Link: https://adventofcode.com/2023/day/18
  Difficulty: l
  Tags: geometry
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  import AdventOfCode.Algorithms.Geometry, only: [polygon_area: 1]

  @dir_map_1 %{"R" => :right, "D" => :down, "L" => :left, "U" => :up}
  @dir_map_2 %{"0" => :right, "1" => :down, "2" => :left, "3" => :up}

  def input, do: InputReader.read_from_file(2023, 18)

  def run(input \\ input()) do
    input = parse(input)
    {input |> skip_colour() |> lava_capacity(), input |> rectify() |> lava_capacity()}
  end

  def parse(data \\ input()) do
    for line <- Transformers.lines(data) do
      [_, dir, steps, colours] = Regex.run(~r{(.) (\d+) \((.+)\)}, line)
      {@dir_map_1[dir], String.to_integer(steps), colours}
    end
  end

  defp lava_capacity(input) do
    {perimeter, polygon} = get_geometry(input)
    trunc(polygon_area(polygon) + div(perimeter, 2) + 1)
  end

  defp move(rules), do: move(rules, {0, 0}, [{0, 0}])
  defp move([], _, points), do: Enum.reverse(points)

  defp move([{:up, steps} | rules], {x, y}, points),
    do: move(rules, {x - steps, y}, [{x - steps, y} | points])

  defp move([{:down, steps} | rules], {x, y}, points),
    do: move(rules, {x + steps, y}, [{x + steps, y} | points])

  defp move([{:left, steps} | rules], {x, y}, points),
    do: move(rules, {x, y - steps}, [{x, y - steps} | points])

  defp move([{:right, steps} | rules], {x, y}, points),
    do: move(rules, {x, y + steps}, [{x, y + steps} | points])

  defp get_geometry(instructions) do
    perimeter = Enum.reduce(instructions, 0, fn x, acc -> acc + elem(x, 1) end)

    {perimeter, move(instructions)}
  end

  defp process_colour(<<"#", steps::binary-size(5), dir::binary-1>>),
    do: {@dir_map_2[dir], elem(Integer.parse(steps, 16), 0)}

  defp skip_colour(instructions), do: Enum.map(instructions, fn {a, b, _} -> {a, b} end)
  defp rectify(instructions), do: Enum.map(instructions, &process_colour(elem(&1, 2)))
end
