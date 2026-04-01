defmodule AdventOfCode.Y2017.Day18 do
  @moduledoc """
  --- Day 18: Duet ---
  Problem Link: https://adventofcode.com/2017/day/18
  Difficulty: m
  Tags: data-modelling op-code
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 18)

  def run(input \\ input()) do
    instructions = parse(input)
    {run_1(instructions), run_2(instructions)}
  end

  defp run_1(instructions) do
    machine = %{instr: instructions, seq: 0, regs: %{}, last_snd: nil}
    execute_part1(machine)
  end

  defp execute_part1(%{instr: instr, seq: seq} = m) do
    case instr[seq] do
      {:snd, src} ->
        execute_part1(%{m | last_snd: get_val(m.regs, src), seq: seq + 1})

      {:rcv, src} ->
        if get_val(m.regs, src) != 0, do: m.last_snd, else: execute_part1(%{m | seq: seq + 1})

      instruction when is_tuple(instruction) ->
        case execute_common(m, instruction) do
          {:ok, m} -> execute_part1(m)
          :halt -> :error
        end

      nil ->
        :error
    end
  end

  defp run_2(instructions) do
    s0 = %{instr: instructions, seq: 0, regs: %{"p" => 0}, queue: []}
    s1 = %{instr: instructions, seq: 0, regs: %{"p" => 1}, queue: []}
    simulate(s0, s1, 0)
  end

  defp simulate(s0, s1, sent_count) do
    {s0, out0, stat0} = run_program(s0)
    {s1, out1, stat1} = run_program(s1)

    s1 = %{s1 | queue: s1.queue ++ out0}
    s0 = %{s0 | queue: s0.queue ++ out1}
    new_sent_count = sent_count + length(out1)

    if (out0 == [] and out1 == [] and is_deadlock(s0, s1, stat0, stat1)) or
         (stat0 == :halted and stat1 == :halted) do
      new_sent_count
    else
      simulate(s0, s1, new_sent_count)
    end
  end

  defp is_deadlock(s0, s1, stat0, stat1) do
    (stat0 == :blocked and s0.queue == [] and (stat1 == :blocked and s1.queue == [])) or
      (stat0 == :halted and stat1 == :blocked and s1.queue == []) or
      (stat1 == :halted and stat0 == :blocked and s0.queue == [])
  end

  defp run_program(%{instr: instr, seq: seq} = m, out \\ []) do
    case instr[seq] do
      {:snd, src} ->
        run_program(%{m | seq: seq + 1}, out ++ [get_val(m.regs, src)])

      {:rcv, {:ref, reg}} ->
        case m.queue do
          [val | rest] ->
            run_program(%{m | regs: Map.put(m.regs, reg, val), queue: rest, seq: seq + 1}, out)

          [] ->
            {m, out, :blocked}
        end

      instruction when is_tuple(instruction) ->
        case execute_common(m, instruction) do
          {:ok, m} -> run_program(m, out)
          :halt -> {m, out, :halted}
        end

      nil ->
        {m, out, :halted}
    end
  end

  defp execute_common(%{regs: regs, seq: seq} = m, instruction) do
    case instruction do
      {:set, reg, src} ->
        {:ok, %{m | regs: Map.put(regs, reg, get_val(regs, src)), seq: seq + 1}}

      {:add, reg, src} ->
        {:ok,
         %{
           m
           | regs: Map.update(regs, reg, get_val(regs, src), &(&1 + get_val(regs, src))),
             seq: seq + 1
         }}

      {:mul, reg, src} ->
        {:ok, %{m | regs: Map.update(regs, reg, 0, &(&1 * get_val(regs, src))), seq: seq + 1}}

      {:mod, reg, src} ->
        {:ok, %{m | regs: Map.update(regs, reg, 0, &rem(&1, get_val(regs, src))), seq: seq + 1}}

      {:jgz, src, offset} ->
        jump = if get_val(regs, src) > 0, do: get_val(regs, offset), else: 1
        {:ok, %{m | seq: seq + jump}}

      _ ->
        :halt
    end
  end

  defp get_val(_regs, {:val, v}), do: v
  defp get_val(regs, {:ref, r}), do: Map.get(regs, r, 0)

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.with_index()
    |> Map.new(fn {line, idx} ->
      case Transformers.words(line) do
        ["snd", src] -> {idx, {:snd, parse_rval(src)}}
        ["rcv", src] -> {idx, {:rcv, parse_rval(src)}}
        ["set", reg, src] -> {idx, {:set, reg, parse_rval(src)}}
        ["add", reg, src] -> {idx, {:add, reg, parse_rval(src)}}
        ["mul", reg, src] -> {idx, {:mul, reg, parse_rval(src)}}
        ["mod", reg, src] -> {idx, {:mod, reg, parse_rval(src)}}
        ["jgz", reg, src] -> {idx, {:jgz, parse_rval(reg), parse_rval(src)}}
      end
    end)
  end

  defp parse_rval(value) do
    case Integer.parse(value) do
      {n, ""} -> {:val, n}
      _ -> {:ref, value}
    end
  end
end
