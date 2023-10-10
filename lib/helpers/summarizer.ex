defmodule AdventOfCode.Helpers.Summarizer do
  alias AdventOfCode.Helpers.Meta

  @year_range 2015..2022

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
        | #{day} | [#{line.title}](#{line.link}) | #{award(line.count)} | #{linkify(line.solution)}] | #{linkify(line.test)} |"
        """
      end

    """
    #{heading}

    #{trophy}

    #{table_header}\n
    | :---: | :------: | ---: | :---: |\n
    #{table_content |> Enum.join("\n")}
    """
  end

  defp award(1), do: ":trophy:"
  defp award(2), do: ":trophy: :trophy:"

  defp linkify(link) do
    file = link |> String.split("/") |> List.last()
    "[#{file}](#{link})"
  end
end
