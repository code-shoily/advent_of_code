defmodule AdventOfCode.Y2016.Day01Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1601

  alias AdventOfCode.Y2016.Day01, as: Solution

  test "Year 2016, Day 1, Part 1" do
    assert Solution.run() == {253, 126}
  end
end
