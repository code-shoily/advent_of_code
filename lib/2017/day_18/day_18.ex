defmodule AdventOfCode.Y2017.Day18 do
  @moduledoc """
  --- Day 18: Duet ---
  Problem Link: https://adventofcode.com/2017/day/18
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 18

  alias AdventOfCode.Helpers.Transformers
  alias __MODULE__.Duet

  def run(input \\ input!()) do
    instructions = parse(input)

    {run_1(instructions), run_2(instructions)}
  end

  defp run_1(instructions) do
    perform_until_recovery(%Duet{instructions: instructions})
  end

  defp perform_until_recovery(%Duet{} = duet) do
    case follow_instruction(duet) do
      %Duet{} = duet -> perform_until_recovery(duet)
      recovery -> recovery
    end
  end

  defp follow_instruction(%Duet{instructions: instructions, seq: seq} = duet) do
    case instructions[seq] do
      {:snd, frequency} -> Duet.play_sound(duet, frequency)
      {:rcv, source} -> Duet.recover_last(duet, source)
      {:set, to, from} -> Duet.set_value(duet, to, from)
      {:add, register, source} -> Duet.binary_op(duet, :+, register, source)
      {:mul, register, source} -> Duet.binary_op(duet, :*, register, source)
      {:mod, register, source} -> Duet.binary_op(duet, :rem, register, source)
      {:jgz, register, step_source} -> Duet.jump_if_nonzero(duet, register, step_source)
    end
  end

  defp run_2(_input) do
    {:todo, 2}
  end

  def parse(data \\ input!()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      case Transformers.words(line) do
        ["snd", frequency] -> {:snd, parse_rval(frequency)}
        ["rcv", frequency] -> {:rcv, parse_rval(frequency)}
        ["set", reg, value] -> {:set, reg, parse_rval(value)}
        ["add", reg, value] -> {:add, reg, parse_rval(value)}
        ["mul", reg, value] -> {:mul, reg, parse_rval(value)}
        ["mod", reg, value] -> {:mod, reg, parse_rval(value)}
        ["jgz", reg, value] -> {:jgz, reg, parse_rval(value)}
      end
    end)
    |> Enum.with_index()
    |> Map.new(fn {v, k} -> {k, v} end)
  end

  defp parse_rval(value) do
    case Integer.parse(value) do
      {value, ""} -> {:val, value}
      :error -> {:ref, value}
    end
  end
end
