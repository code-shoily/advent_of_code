defmodule AdventOfCode.Y2015.Day6 do
  use AdventOfCode.Data.InputReader, year: 2015, day: 6

  def init(size) do
    for i <- 0..size, into: %{} do
      {i, for(j <- 0..size, into: %{}, do: {j, false})}
    end
  end

  def execute({:on, [[x1, y1], [x2, y2]]}, grid) do
  end

  def execute({:off, [[x1, y1], [x2, y2]]}, grid) do
  end

  def execute({:toggle, [[x1, y1], [x2, y2]]}, grid) do
  end

  def coordinates(data) do
    data
    |> String.trim()
    |> String.split(" ")
    |> Enum.filter(&String.contains?(&1, ","))
    |> Enum.map(&String.split(&1, ","))
    |> Enum.map(fn [x, y] ->
      [String.to_integer(x), String.to_integer(y)]
    end)
  end

  def parse_instruction("turn on" <> rest), do: {:on, coordinates(rest)}
  def parse_instruction("turn off" <> rest), do: {:off, coordinates(rest)}
  def parse_instruction("toggle" <> rest), do: {:toggle, coordinates(rest)}

  def parse_line(line) do
    line
    |> String.replace("through", "")
    |> parse_instruction()
  end

  def run do
    grid = init()

    input!()
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
  end
end
