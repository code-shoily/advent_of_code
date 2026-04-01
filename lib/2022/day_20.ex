defmodule AdventOfCode.Y2022.Day20 do
  @moduledoc """
  --- Day 20: Grove Positioning System ---
  Problem Link: https://adventofcode.com/2022/day/20
  Difficulty: m
  Tags: sequence large-number circular-list
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 20)

  def run(input \\ input()) do
    input = parse(input)

    run_1_task = Task.async(fn -> compute_grove_sum(input, 1) end)

    run_2_task =
      Task.async(fn ->
        input
        |> Enum.map(fn {v, i} -> {v * 811_589_153, i} end)
        |> compute_grove_sum(10)
      end)

    {Task.await(run_1_task, :infinity), Task.await(run_2_task, :infinity)}
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index()
  end

  defp compute_grove_sum(input, repeat) do
    n = length(input)
    # Map original index to value
    values = Map.new(input, fn {v, i} -> {i, v} end)

    # Linked list using maps of original indices
    {prevs, nexts} = init_list(n)

    # Mixing
    {_prevs, nexts} =
      Enum.reduce(1..repeat, {prevs, nexts}, fn _, acc ->
        Enum.reduce(0..(n - 1), acc, fn i, {p_acc, n_acc} ->
          mix_one(i, values[i], n, p_acc, n_acc)
        end)
      end)

    # Zero node (the original index that has value 0)
    {zero_idx, _} = Enum.find(values, fn {_, v} -> v == 0 end)

    extract_grove_sum(zero_idx, values, nexts, n)
  end

  defp init_list(n) do
    prevs = Map.new(0..(n - 1), fn i -> {i, Integer.mod(i - 1, n)} end)
    nexts = Map.new(0..(n - 1), fn i -> {i, Integer.mod(i + 1, n)} end)
    {prevs, nexts}
  end

  defp mix_one(i, val, n, prevs, nexts) do
    steps = Integer.mod(val, n - 1)

    if steps == 0 do
      {prevs, nexts}
    else
      p = Map.fetch!(prevs, i)
      nex = Map.fetch!(nexts, i)

      # Remove i
      prevs = Map.put(prevs, nex, p)
      nexts = Map.put(nexts, p, nex)

      # Find insertion point
      target =
        if steps <= div(n, 2) do
          Enum.reduce(1..steps, p, fn _, curr -> Map.fetch!(nexts, curr) end)
        else
          Enum.reduce(1..(n - 1 - steps), p, fn _, curr -> Map.fetch!(prevs, curr) end)
        end

      # Insert i after target
      after_target = Map.fetch!(nexts, target)

      prevs = prevs |> Map.put(i, target) |> Map.put(after_target, i)
      nexts = nexts |> Map.put(target, i) |> Map.put(i, after_target)

      {prevs, nexts}
    end
  end

  defp extract_grove_sum(zero_idx, values, nexts, n) do
    [1000, 2000, 3000]
    |> Enum.map(fn steps ->
      steps = rem(steps, n)
      node = Enum.reduce(1..steps, zero_idx, fn _, curr -> Map.fetch!(nexts, curr) end)
      Map.fetch!(values, node)
    end)
    |> Enum.sum()
  end
end
