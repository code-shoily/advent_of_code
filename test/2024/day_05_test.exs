defmodule AdventOfCode.Y2024.Day05Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2405

  alias AdventOfCode.Y2024.Day05, as: Solution

  test "Year 2024, Day 5 run/1" do
    assert Solution.run() == {5391, 6142}
  end
end
