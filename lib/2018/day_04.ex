defmodule AdventOfCode.Y2018.Day04 do
  @moduledoc """
  --- Day 4: Repose Record ---
  Problem Link: https://adventofcode.com/2018/day/4
  """
  use AdventOfCode.Helpers.InputReader, year: 2018, day: 4

  def run_1 do
    store = input!() |> parse() |> sleep_time()

    most_sleeper =
      store
      |> Enum.max_by(& &1[:logs][:duration])
      |> Map.get(:id)

    most_minute =
      store
      |> Enum.group_by(& &1[:id])
      |> Map.get(most_sleeper)
      |> hd()
      |> get_in([:logs, :minutes])
      |> get_mode()
      |> elem(0)

    most_minute * most_sleeper
  end

  def run_2 do
    input!()
    |> parse()
    |> sleep_time()
    |> Enum.map(fn %{id: id, logs: %{minutes: m}} -> %{id: id, minutes: get_mode(m)} end)
    |> Enum.max_by(&elem(&1[:minutes], 1))
    |> then(fn %{id: id, minutes: {minute, _}} -> id * minute end)
  end

  def parse(data) do
    data
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Enum.sort(fn a, b -> NaiveDateTime.diff(a[:timestamp], b[:timestamp]) < 0 end)
    |> normalize()
  end

  @regex ~r"""
    ^\[(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})\s
       (?<hour>\d{2}):(?<minute>\d{2})\]
      \s(?<log>.+)$
  """x
  defp parse_line(line) do
    @regex
    |> Regex.named_captures(line)
    |> sanitize()
  end

  defp sanitize(%{
         "year" => year,
         "day" => day,
         "month" => month,
         "hour" => hour,
         "minute" => minute,
         "log" => log
       }) do
    %{
      timestamp: to_datetime(year, month, day, hour, minute),
      day: String.to_integer(day),
      month: String.to_integer(month),
      hour: String.to_integer(hour),
      minute: String.to_integer(minute),
      log: parse_log(log)
    }
  end

  defp to_datetime(year, month, day, hour, minute) do
    {:ok, result} = NaiveDateTime.from_iso8601("#{year}-#{month}-#{day} #{hour}:#{minute}:00Z")
    result
  end

  defp xid(log), do: String.split(log, " ") |> hd()
  defp parse_log("wakes up"), do: {:wakes}
  defp parse_log("falls asleep"), do: {:sleeps}
  defp parse_log("Guard #" <> rst), do: {:begins, xid(rst)}

  defp normalize(dataset) do
    dataset
    |> Enum.reduce(%{id: nil, data: []}, fn log, %{id: id, data: data} = acc ->
      case log do
        %{log: {:begins, current_id}} -> %{acc | id: current_id, data: [log | data]}
        %{log: {action}} -> %{acc | data: [%{log | log: {action, id}} | data]}
      end
    end)
    |> Map.get(:data)
    |> Enum.reverse()
    |> Enum.map(fn %{log: {action, id}} = log ->
      log
      |> Map.merge(%{action: action, id: id})
      |> Map.delete(:log)
    end)
  end

  def duration(start, stop) do
    stop
    |> NaiveDateTime.add(-60, :second)
    |> NaiveDateTime.diff(start)
  end

  def minutes(t1, t2), do: Enum.to_list(t1.minute..(t2.minute - 1))

  defp sleep_time(data) do
    data
    |> Enum.reject(fn %{action: action} -> action == :begins end)
    |> Enum.map(&Map.take(&1, [:id, :timestamp]))
    |> Enum.chunk_every(2)
    |> Enum.map(fn [sleeps, wakes] ->
      %{
        id: String.to_integer(sleeps[:id]),
        duration: duration(sleeps[:timestamp], wakes[:timestamp]),
        minutes: minutes(sleeps[:timestamp], wakes[:timestamp])
      }
    end)
    |> Enum.group_by(& &1[:id])
    |> Enum.map(fn {id, logs} ->
      logs =
        logs
        |> Enum.reduce(
          %{duration: 0, minutes: []},
          fn log, %{duration: duration, minutes: minutes} = acc ->
            %{acc | duration: duration + log[:duration], minutes: minutes ++ log[:minutes]}
          end
        )

      %{id: id, logs: logs}
    end)
  end

  defp get_mode(lst) do
    mode_line =
      lst
      |> Enum.sort()
      |> Enum.chunk_by(& &1)
      |> Enum.max_by(&length(&1))

    {hd(mode_line), length(mode_line)}
  end
end
