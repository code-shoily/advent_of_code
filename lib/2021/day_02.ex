defmodule AdventOfCode.Y2021.Day02 do
  @moduledoc """
  --- Day 2: Dive! ---
  Problem Link: https://adventofcode.com/2021/day/2
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 2

  def run_1, do: input!() |> parse() |> track_positions() |> then(&(&1.depth * &1.horizontal))
  def run_2, do: input!() |> parse() |> track_aims() |> then(&(&1.depth * &1.horizontal))

  def parse(data) do
    data
    |> String.split("\n")
    |> Enum.map(fn line ->
      [direction, value] = String.split(line, " ")
      {String.to_existing_atom(direction), String.to_integer(value)}
    end)
  end

  defp track_positions(directions) do
    directions
    |> Enum.reduce(%{horizontal: 0, depth: 0}, fn
      {:forward, v}, %{horizontal: horizontal} = acc -> %{acc | horizontal: horizontal + v}
      {:backward, v}, %{horizontal: horizontal} = acc -> %{acc | horizontal: horizontal - v}
      {:up, v}, %{depth: depth} = acc -> %{acc | depth: depth - v}
      {:down, v}, %{depth: depth} = acc -> %{acc | depth: depth + v}
    end)
  end

  defp track_aims(directions) do
    directions
    |> Enum.reduce(%{horizontal: 0, depth: 0, aim: 0}, fn
      {:forward, v}, %{horizontal: horizontal, depth: depth, aim: aim} = acc ->
        %{acc | horizontal: horizontal + v, depth: depth + aim * v}

      {:backward, v}, %{horizontal: horizontal} = acc ->
        %{acc | horizontal: horizontal - v}

      {:up, v}, %{aim: aim} = acc ->
        %{acc | aim: aim - v}

      {:down, v}, %{aim: aim} = acc ->
        %{acc | aim: aim + v}
    end)
  end
end