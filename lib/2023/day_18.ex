defmodule AdventOfCode.Y2023.Day18 do
  @moduledoc """
  --- Day 18: Lavaduct Lagoon ---
  Problem Link: https://adventofcode.com/2023/day/18
  Difficulty: l
  Tags: geometry
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  import AdventOfCode.Algorithms.Geometry, only: [polygon_area: 1]

  @dir_mapper %{
    "R" => :right,
    "D" => :down,
    "L" => :left,
    "U" => :up,
    "0" => :right,
    "1" => :down,
    "2" => :left,
    "3" => :up
  }
  @origin {0, 0}

  def input, do: InputReader.read_from_file(2023, 18)

  def run(input \\ input()) do
    input = parse(input)
    {lava_capacity(input), lava_capacity(correct(input))}
  end

  defp lava_capacity(input) do
    {perimeter, polygon} = get_geometry(input)
    trunc(polygon_area(polygon) + div(perimeter, 2) + 1)
  end

  def move(rules), do: move(rules, @origin, [@origin])
  def move([], _, points), do: Enum.reverse(points)

  def move([{:up, steps, _} | rules], {x, y}, points),
    do: move(rules, {x - steps, y}, [{x - steps, y} | points])

  def move([{:down, steps, _} | rules], {x, y}, points),
    do: move(rules, {x + steps, y}, [{x + steps, y} | points])

  def move([{:left, steps, _} | rules], {x, y}, points),
    do: move(rules, {x, y - steps}, [{x, y - steps} | points])

  def move([{:right, steps, _} | rules], {x, y}, points),
    do: move(rules, {x, y + steps}, [{x, y + steps} | points])

  def parse(data \\ input()) do
    for line <- Transformers.lines(data) do
      [_, dir, steps, colours] = Regex.run(~r{(.) (\d+) \((.+)\)}, line)
      {@dir_mapper[dir], String.to_integer(steps), colours}
    end
  end

  def get_geometry(instructions) do
    perimeter = Enum.reduce(instructions, 0, fn x, acc -> acc + elem(x, 1) end)

    {perimeter, move(instructions)}
  end

  def process_colour(<<"#", steps::binary-size(5), dir::binary-1>>),
    do: {@dir_mapper[dir], elem(Integer.parse(steps, 16), 0), :discard}

  def correct(instructions), do: Enum.map(instructions, &process_colour(elem(&1, 2)))
end
