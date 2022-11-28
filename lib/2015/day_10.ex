defmodule AdventOfCode.Y2015.Day10 do
  @moduledoc """
  --- Day 10: Elves Look, Elves Say ---
  Problem Link: https://adventofcode.com/2015/day/10
  """
  alias AdventOfCode.Helpers.InputReader

  @input InputReader.read_from_file(2015, 10)

  def run(input \\ @input), do: look_and_say(String.graphemes(input), 40, 50)

  def encode(input), do: Enum.reverse(encode(input, nil, nil, []))
  def encode([], b, n, r), do: [b, to_string(n) | r]
  def encode([a | input], nil, nil, []), do: encode(input, a, 1, [])
  def encode([a | input], a, n, r), do: encode(input, a, n + 1, r)
  def encode([a | input], b, n, r), do: encode(input, a, 1, [b, to_string(n) | r])

  defp look_and_say(input, first_stop, second_stop) do
    1..second_stop
    |> Enum.reduce({nil, input}, fn x, {a, b} ->
      case {x, encode(b)} do
        {^first_stop, data} -> {data, data}
        {_, data} -> {a, data}
      end
    end)
    |> then(fn {a, b} -> {length(a), length(b)} end)
  end
end
