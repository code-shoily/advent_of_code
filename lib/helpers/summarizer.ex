defmodule AdventOfCode.Helpers.Summarizer do
  alias AdventOfCode.Helpers.Meta

  @year_range 2015..2022

  def summarize() do
    metadata = Map.new(@year_range, &{&1, Meta.get_info(&1, true)})

    header = "| Day | [2015](/lib/2015) | [2016](/lib/2016) | [2017](/lib/2017) | [2018](/lib/2018) | [2019](/lib/2019) | [2020](/lib/2020) | [2021](lib/2021) | [2022](lib/2022) |"
    content =
      for i <- 0..25 do
        generate_stat_row(metadata, i)
      end

    """
    #{header}
    |:---:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
    #{content}
    """
  end

  def generate_stat_row(metadata, 0) do
    stars =
      for year <- @year_range do
        metadata[year].completed
      end
      |> Enum.join(" | ")
    """
    | :star2: | #{stars} |
    """
  end

  def generate_stat_row(metadata, day) do
    counts =
      for year <- @year_range do
        case metadata[year].summary[day] do
          nil -> " "
          %{count: count} -> award(count)
        end
      end
      |> Enum.join(" | ")

    """
    | #{day} | #{counts} |
    """
  end

  def build_heading(year) do
    links =
      for current_year <- @year_range do
        if current_year == year do
          to_string(current_year)
        else
          "[#{current_year}](/lib/#{current_year})"
        end
      end

    """
    <!-- AUTOGENERATED -- DO NOT EDIT -- use `mix gen_stats` -->
    # Advent of Code #{year}

    [Main Page](https://adventofcode.com/#{year}) | [Tests](/test/#{year})

    #{links |> Enum.join(" | ")}
    """
  end

  def yearwise_readme(year) do
    heading = build_heading(year)
    table_header = "| Day | Problem Page | Status | Solution Page | Test Page |"
    info = Meta.get_info(year)
    trophy = "## :trophy: #{info.completed}/50"

    table_content =
      for {day, line} <- info.summary do
        """
        | #{day} | [#{line.title}](#{line.link}) | #{award(line.count)} | #{linkify(line.solution)} | #{linkify(line.test)} |
        """
      end

    """
    #{heading}

    #{trophy}

    #{table_header}
    | :---: | :------: | ---: | :---: | :---: |
    #{table_content}
    """
  end

  defp award(1), do: ":2nd_place_medal:"
  defp award(2), do: ":1st_place_medal:"

  defp linkify(link) do
    file = link |> String.split("/") |> List.last()
    "[#{file}](#{link})"
  end
end
