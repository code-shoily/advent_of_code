defmodule AdventOfCode.Y2023.Day20 do
  @moduledoc """
  --- Day 20: Pulse Propagation ---
  Problem Link: https://adventofcode.com/2023/day/20
  Difficulty: l
  Tags: simulation logic-circuit graph cycles lcm
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 20)

  def run(input \\ input()) do
    {modules, graph} = parse_modules(input)

    {run_1(modules, graph), run_2(modules, graph)}
  end

  defp run_1(modules, graph) do
    state = init_state(modules, graph)

    {_final_state, {lows, highs}} =
      Enum.reduce(1..1000, {state, {0, 0}}, fn _, {curr_state, {l, h}} ->
        {next_state, {new_l, new_h}} =
          send_pulse(curr_state, modules, {"button", "broadcaster", :low})

        {next_state, {l + new_l, h + new_h}}
      end)

    lows * highs
  end

  defp run_2(modules, graph) do
    hb = graph |> Map.get("rx") |> hd()
    ancestors = Map.get(graph, hb, [])

    state = init_state(modules, graph)

    periods =
      Enum.reduce_while(Stream.iterate(1, &(&1 + 1)), {state, %{}}, fn press,
                                                                       {curr_state, found} ->
        {next_state, high_events} =
          send_pulse_with_tracking(curr_state, modules, {"button", "broadcaster", :low})

        new_found =
          Enum.reduce(high_events, found, fn {from, _to, pulse}, acc ->
            if pulse == :high and from in ancestors and !Map.has_key?(acc, from) do
              Map.put(acc, from, press)
            else
              acc
            end
          end)

        if map_size(new_found) == length(ancestors) do
          {:halt, new_found}
        else
          {:cont, {next_state, new_found}}
        end
      end)

    periods |> Map.values() |> Enum.reduce(1, &lcm/2)
  end

  defp send_pulse(state, modules, initial_pulse) do
    q = :queue.from_list([initial_pulse])
    process_pulses(q, state, modules, {0, 0})
  end

  defp process_pulses(q, state, modules, {l, h}) do
    case :queue.out(q) do
      {:empty, _} ->
        {state, {l, h}}

      {{:value, {from, to, pulse}}, rest} ->
        {new_l, new_h} = if pulse == :low, do: {l + 1, h}, else: {l, h + 1}

        case Map.get(modules, to) do
          nil ->
            process_pulses(rest, state, modules, {new_l, new_h})

          %{type: :broadcaster, dests: dests} ->
            new_pulses = Enum.map(dests, fn d -> {to, d, pulse} end)
            process_pulses(enqueue_list(rest, new_pulses), state, modules, {new_l, new_h})

          %{type: :flip_flop, dests: dests} ->
            if pulse == :high do
              process_pulses(rest, state, modules, {new_l, new_h})
            else
              curr_on = Map.get(state, to)
              new_state = Map.put(state, to, !curr_on)
              new_pulse = if curr_on, do: :low, else: :high
              new_pulses = Enum.map(dests, fn d -> {to, d, new_pulse} end)
              process_pulses(enqueue_list(rest, new_pulses), new_state, modules, {new_l, new_h})
            end

          %{type: :conjunction, dests: dests} ->
            conj_state = Map.get(state, to) |> Map.put(from, pulse)
            new_state = Map.put(state, to, conj_state)

            output_pulse =
              if Enum.all?(conj_state, fn {_, p} -> p == :high end), do: :low, else: :high

            new_pulses = Enum.map(dests, fn d -> {to, d, output_pulse} end)
            process_pulses(enqueue_list(rest, new_pulses), new_state, modules, {new_l, new_h})
        end
    end
  end

  defp send_pulse_with_tracking(state, modules, initial_pulse) do
    q = :queue.from_list([initial_pulse])
    process_pulses_tracking(q, state, modules, [])
  end

  defp process_pulses_tracking(q, state, modules, events) do
    case :queue.out(q) do
      {:empty, _} ->
        {state, Enum.reverse(events)}

      {{:value, {from, to, pulse}}, rest} ->
        new_events = [{from, to, pulse} | events]

        case Map.get(modules, to) do
          nil ->
            process_pulses_tracking(rest, state, modules, new_events)

          %{type: :broadcaster, dests: dests} ->
            new_pulses = Enum.map(dests, fn d -> {to, d, pulse} end)
            process_pulses_tracking(enqueue_list(rest, new_pulses), state, modules, new_events)

          %{type: :flip_flop, dests: dests} ->
            if pulse == :high do
              process_pulses_tracking(rest, state, modules, new_events)
            else
              curr_on = Map.get(state, to)
              new_state = Map.put(state, to, !curr_on)
              new_pulse = if curr_on, do: :low, else: :high
              new_pulses = Enum.map(dests, fn d -> {to, d, new_pulse} end)

              process_pulses_tracking(
                enqueue_list(rest, new_pulses),
                new_state,
                modules,
                new_events
              )
            end

          %{type: :conjunction, dests: dests} ->
            conj_state = Map.get(state, to) |> Map.put(from, pulse)
            new_state = Map.put(state, to, conj_state)

            output_pulse =
              if Enum.all?(conj_state, fn {_, p} -> p == :high end), do: :low, else: :high

            new_pulses = Enum.map(dests, fn d -> {to, d, output_pulse} end)

            process_pulses_tracking(
              enqueue_list(rest, new_pulses),
              new_state,
              modules,
              new_events
            )
        end
    end
  end

  defp enqueue_list(q, list) do
    Enum.reduce(list, q, fn item, acc -> :queue.in(item, acc) end)
  end

  defp init_state(modules, graph) do
    Enum.reduce(modules, %{}, fn
      {name, %{type: :flip_flop}}, acc ->
        Map.put(acc, name, false)

      {name, %{type: :conjunction}}, acc ->
        inputs = Map.get(graph, name, [])
        Map.put(acc, name, Map.new(inputs, &{&1, :low}))

      _, acc ->
        acc
    end)
  end

  defp parse_modules(data) do
    modules =
      data
      |> Transformers.lines()
      |> Enum.map(fn line ->
        [src_raw, dests_raw] = String.split(line, " -> ")
        dests = String.split(dests_raw, ", ")

        {name, type} =
          cond do
            String.starts_with?(src_raw, "%") ->
              {String.replace_prefix(src_raw, "%", ""), :flip_flop}

            String.starts_with?(src_raw, "&") ->
              {String.replace_prefix(src_raw, "&", ""), :conjunction}

            src_raw == "broadcaster" ->
              {"broadcaster", :broadcaster}
          end

        {name, %{type: type, dests: dests}}
      end)
      |> Map.new()

    edges = for {name, %{dests: ds}} <- modules, d <- ds, do: {d, name}
    graph = Enum.group_by(edges, &elem(&1, 0), &elem(&1, 1))

    {modules, graph}
  end

  defp gcd(a, 0), do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))
  defp lcm(a, b), do: div(abs(a * b), gcd(a, b))

  def parse(data \\ input()) do
    data |> Transformers.lines()
  end
end
