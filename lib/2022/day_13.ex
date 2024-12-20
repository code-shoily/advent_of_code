defmodule AdventOfCode.Y2022.Day13 do
  @moduledoc """
  --- Day 13: Distress Signal ---
  Problem Link: https://adventofcode.com/2022/day/13
  Difficulty: s
  Tags: json sequence
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 13)

  def run(input \\ input()), do: {run_1(input), run_2(input)}

  def run_1(input) do
    input
    |> String.split(~r{(\r\n\r\n|\r\r|\n\n)})
    |> Enum.map(fn pair ->
      [a, b] = Transformers.lines(pair)
      {JSON.decode!(a), JSON.decode!(b)}
    end)
    |> Enum.with_index(1)
    |> Enum.filter(fn {{x, y}, _} -> order(x, y) end)
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  def run_2(input) do
    input
    |> String.split(~r{(\r\n\r\n|\r\r|\n\n)})
    |> Enum.flat_map(&Transformers.lines(&1))
    |> Enum.map(&JSON.decode!/1)
    |> Kernel.++([[[2]], [[6]]])
    |> Enum.sort_by(&Function.identity/1, &order/2)
    |> Enum.with_index(1)
    |> Enum.filter(fn {elem, _} -> elem in [[[2]], [[6]]] end)
    |> Enum.map(&elem(&1, 1))
    |> Enum.product()
  end

  def order([], []), do: nil
  def order([_ | _], []), do: false
  def order([], [_ | _]), do: true

  def order([x | x_rest], [y | y_rest]) when is_integer(x) and is_integer(y),
    do: (x == y && order(x_rest, y_rest)) || x < y

  def order([x | x_rest], [y | y_rest]) when is_list(x) and is_list(y) do
    case order(x, y) do
      nil -> order(x_rest, y_rest)
      val -> val
    end
  end

  def order([x | x_rest], [y | y_rest]) when is_list(x) and is_integer(y) do
    order([x | x_rest], [[y] | y_rest])
  end

  def order([x | x_rest], [y | y_rest]) when is_integer(x) and is_list(y) do
    order([[x] | x_rest], [y | y_rest])
  end
end
