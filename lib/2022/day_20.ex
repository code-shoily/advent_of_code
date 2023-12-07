defmodule AdventOfCode.Y2022.Day20 do
  @moduledoc """
  --- Day 20: Grove Positioning System ---
  Problem Link: https://adventofcode.com/2022/day/20
  Difficulty: m
  Tags: slow vector sequence large-number random-access
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Aja.Vector

  def input, do: InputReader.read_from_file(2022, 20)

  def run(input \\ input()) do
    input = parse(input)

    run_1 = Task.async(fn -> run_1(input) end)
    run_2 = Task.async(fn -> run_2(input) end)

    {Task.await(run_1, :infinity), Task.await(run_2, :infinity)}
  end

  defp run_1(input) do
    compute_grove_sum(input, 1)
  end

  @decryption_key 811_589_153
  def run_2(input) do
    input
    |> Vector.map(fn {v, i} -> {v * @decryption_key, i} end)
    |> compute_grove_sum(10)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index()
    |> Vector.new()
  end

  defp compute_grove_sum(input, repeat) do
    n = Vector.size(input) - 1
    sequence = mix(input, n, repeat)
    zero_idx = index_of(sequence, n, 0)
    grove_sum(sequence, n, zero_idx)
  end

  defp grove_sum(sequence, n, zero_idx) do
    Enum.reduce(1..3, 0, fn x, acc ->
      {val, _} = sequence[rem(zero_idx + x * 1000, n)]
      acc + val
    end)
  end

  defp mix(input, n, repeat) do
    1..repeat
    |> Enum.reduce(input, fn _, repeated_acc ->
      0..n
      |> Enum.reduce(repeated_acc, fn i, acc ->
        j = value_of(acc, n, i)
        {val, _} = Vector.at(acc, j)
        {_, popped} = Vector.pop_at(acc, j)
        ins = (j + val) |> Integer.mod(n)
        insert_at(popped, ins, {val, i})
      end)
    end)
  end

  defp insert_at(vector, idx, value) do
    {left, right} = Vector.split(vector, idx)
    left |> Vector.append(value) |> Vector.concat(right)
  end

  defp value_of(sequence, n, idx) do
    Enum.reduce_while(0..n, nil, fn val, _ ->
      case sequence[val] do
        {_, ^idx} -> {:halt, val}
        _ -> {:cont, nil}
      end
    end)
  end

  defp index_of(sequence, n, idx) do
    Enum.reduce_while(0..(n - 1), nil, fn x, _ ->
      case sequence[x] do
        {^idx, _} -> {:halt, x}
        _ -> {:cont, nil}
      end
    end)
  end
end
