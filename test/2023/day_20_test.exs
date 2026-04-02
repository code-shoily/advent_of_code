defmodule AdventOfCode.Y2023.Day20Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2320

  alias AdventOfCode.Y2023.Day20, as: Solution

  test "Year 2023, Day 20 run/1" do
    assert Solution.run() == {684_125_385, 225_872_806_380_073}
  end
end
