defmodule AdventOfCode.Y2020.Day9 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/9
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 9

  def run_1, do: input!() |> process() |> find_invalid()
  def run_2, do: input!() |> process() |> contiguous_list()

  def process(input), do: Enum.map(String.split(input, "\n"), &String.to_integer/1)

  defp find_invalid(data) do
    {frame, [v | _] = next} = Enum.split(data, 25)
    (invalid?(v, MapSet.new(frame)) && v) || find_invalid(tl(frame) ++ next)
  end

  def invalid?(value, frame), do: Enum.empty?(Enum.filter(frame, &((value - &1) in frame)))

  defp contiguous_list(data), do: contiguous_list(find_invalid(data), data)

  defp contiguous_list(value, [h | rest]) do
    case contiguous_list(List.wrap(h), rest, value) do
      :bigger -> contiguous_list(value, rest)
      lst -> Enum.max(lst) + Enum.min(lst)
    end
  end

  def contiguous_list(data, [h | rest], value) do
    lst = [h | data]

    case Enum.sum(lst) do
      ^value -> lst
      n when n > value -> :bigger
      _ -> contiguous_list(lst, rest, value)
    end
  end
end
