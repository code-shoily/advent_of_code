defmodule AdventOfCode.Y2020.Day14 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/14

  Hat-tip to Christian Blavier from ElixirForum.
  https://github.com/cblavier/advent/tree/master/lib/2020/day14
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 14

  use Bitwise

  def run_1, do: input!() |> process() |> run(&instruction_1/2) |> memory_sum()
  def run_2, do: input!() |> process() |> run(&instruction_2/2) |> memory_sum()
  def process(input), do: String.split(input, "\n")

  def run(data, instruction), do: Enum.reduce(data, {%{}, nil}, instruction)

  defp instruction_1("mask" <> _ = line, {memory, _mask}) do
    mask = line |> String.split(" = ") |> Enum.at(-1)
    {or_mask, _} = mask |> String.replace("X", "0") |> Integer.parse(2)
    {and_mask, _} = mask |> String.replace("X", "1") |> Integer.parse(2)
    {memory, {or_mask, and_mask}}
  end

  @regex Regex.compile!(~S/mem\[(?<address>\d+)\] = (?<value>\d+)/)
  defp instruction_1(instruction, {memory, masks = {or_mask, and_mask}}) do
    %{"address" => address, "value" => value} = Regex.named_captures(@regex, instruction)
    {address, value} = {String.to_integer(address), String.to_integer(value)}
    {Map.put(memory, address, (value ||| or_mask) &&& and_mask), masks}
  end

  defp memory_sum({memory, _}), do: memory |> Enum.map(&elem(&1, 1)) |> Enum.sum()

  defp instruction_2("mask" <> _ = line, {memory, _}),
    do: {memory, line |> String.split(" = ") |> Enum.at(-1) |> String.graphemes()}

  @regex ~r/mem\[(?<address>\d+)\] = (?<value>\d+)/
  defp instruction_2(instruction, {memory, mask}) do
    %{"address" => address, "value" => value} = Regex.named_captures(@regex, instruction)

    {
      address
      |> String.to_integer()
      |> Integer.to_string(2)
      |> String.pad_leading(length(mask), "0")
      |> String.graphemes()
      |> find_addresses(mask)
      |> Enum.reduce(memory, &Map.put(&2, &1, String.to_integer(value))),
      mask
    }
  end

  defp find_addresses(address, mask) do
    Enum.reduce(Enum.zip(address, mask), [0], fn
      {_, "X"}, acc -> Enum.flat_map(acc, &[&1 * 2, &1 * 2 + 1])
      {_, "1"}, acc -> add_bit(acc, 1)
      {"1", _}, acc -> add_bit(acc, 1)
      _, acc -> add_bit(acc, 0)
    end)
  end

  defp add_bit(acc, bit), do: Enum.map(acc, &(&1 * 2 + bit))
end
