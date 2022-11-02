defmodule AdventOfCode.Y2015.Day14 do
  @moduledoc """
  --- Day 14: Reindeer Olympics ---
  Problem Link: https://adventofcode.com/2015/day/14
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 14

  import AdventOfCode.Helpers.Transformers, only: [transpose: 1]

  @type outcome :: {binary(), stats(), list(non_neg_integer())}
  @type reindeer :: %{
          name: binary(),
          speed: non_neg_integer(),
          duration: non_neg_integer(),
          rest: non_neg_integer()
        }
  @type states :: :resting | :running
  @type stats :: %{
          moving: non_neg_integer(),
          resting: non_neg_integer(),
          distance: non_neg_integer()
        }

  @end_time 2503

  def run_1(input \\ input!()) do
    input
    |> parse()
    |> Enum.map(&race(&1, @end_time))
    |> Enum.max_by(fn {_, %{distance: distance}, _} -> distance end)
    |> then(fn {_, %{distance: distance}, _} -> distance end)
  end

  def run_2(input \\ input!()) do
    input
    |> parse()
    |> Enum.map(&elem(race(&1, @end_time), 2))
    |> transpose()
    |> Enum.map(fn same_second_distance ->
      ahead = Enum.max(same_second_distance)
      Enum.map(same_second_distance, fn distance -> (distance == ahead && 1) || 0 end)
    end)
    |> transpose()
    |> Enum.map(&Enum.sum/1)
    |> Enum.max()
  end

  def parse(data \\ input!()) do
    data
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_stats/1)
  end

  @regex ~r{(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.}
  def parse_stats(line) do
    @regex
    |> Regex.run(line, capture: :all_but_first)
    |> then(fn [name, speed, duration, rest] ->
      %{
        name: name,
        speed: String.to_integer(speed),
        duration: String.to_integer(duration),
        rest: String.to_integer(rest)
      }
    end)
  end

  @spec race(reindeer(), non_neg_integer()) :: outcome()
  def race(reindeer, limit) do
    race(
      reindeer,
      {limit, 0},
      :running,
      %{moving: 0, resting: 0, distance: 0},
      []
    )
  end

  @spec race(
          reindeer(),
          {non_neg_integer(), non_neg_integer()},
          states(),
          stats(),
          list(non_neg_integer())
        ) ::
          outcome()
  def race(%{name: name}, {limit, limit}, _, stats, distances), do: {name, stats, distances}

  def race(
        %{speed: speed, duration: duration} = reindeer,
        {limit, elapsed},
        :running,
        %{moving: moving, distance: distance} = stats,
        distances
      )
      when moving < duration do
    race(
      reindeer,
      {limit, elapsed + 1},
      :running,
      %{stats | moving: moving + 1, distance: distance + speed},
      [distance + speed | distances]
    )
  end

  def race(reindeer, {limit, elapsed}, :running, stats, distances) do
    race(reindeer, {limit, elapsed}, :resting, %{stats | moving: 0}, distances)
  end

  def race(
        %{rest: rest} = reindeer,
        {limit, elapsed},
        :resting,
        %{resting: resting} = stats,
        [distance | _] = distances
      )
      when resting < rest do
    race(
      reindeer,
      {limit, elapsed + 1},
      :resting,
      %{stats | resting: resting + 1},
      [distance | distances]
    )
  end

  def race(reindeer, {limit, elapsed}, :resting, stats, distances) do
    race(reindeer, {limit, elapsed}, :running, %{stats | resting: 0}, distances)
  end
end
