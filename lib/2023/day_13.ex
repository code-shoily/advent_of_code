defmodule AdventOfCode.Y2023.Day13 do
  @moduledoc """
  --- Day 13: Point of Incidence ---
  Problem Link: https://adventofcode.com/2023/day/13
  Difficulty: m
  Tags: sliding-window palindrome
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 13)

  def run(input \\ input()) do
    input = parse(input)

    {solver(input, &(&1 == &2), &mirror?(&1, &2, false)),
     solver(input, &smudge_eq?/2, &mirror?(&1, &2, true))}
  end

  def parse(data \\ input()) do
    for section <- Transformers.sections(data) do
      for line <- Transformers.lines(section) do
        String.graphemes(line)
      end
      |> then(fn original -> {original, Transformers.transpose(original)} end)
    end
  end

  defp solver(input, cmp, mirror?) do
    find_axis = &find_axis(&1, cmp, mirror?)

    for {a, b} <- input, reduce: 0 do
      acc -> acc + find_axis.(a) * 100 + find_axis.(b)
    end
  end

  defp smudge_eq?(x, x), do: true
  defp smudge_eq?(x, y), do: Enum.zip(x, y) |> Enum.count(fn {a, b} -> a != b end) |> Kernel.==(1)

  defp find_axis(xs, eq?, mirror?) do
    xs
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.with_index(1)
    |> Enum.filter(fn {[a, b], _} -> eq?.(a, b) end)
    |> Enum.map(fn {_, idx} ->
      {left, right} = Enum.split(xs, idx)
      (mirror?.(Enum.reverse(left), right) && idx) || 0
    end)
    |> then(&((&1 == [] && 0) || Enum.max(&1)))
  end

  defp mirror?([], _, smudged?), do: !smudged?
  defp mirror?(_, [], smudged?), do: !smudged?
  defp mirror?([x | xs], [x | ys], smudged?), do: mirror?(xs, ys, smudged?)
  defp mirror?([x | xs], [y | ys], true), do: smudge_eq?(x, y) && mirror?(xs, ys, false)
  defp mirror?(_, _, false), do: false
end
