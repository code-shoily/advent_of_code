defmodule AdventOfCode.Y2024.Day03Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2403

  alias AdventOfCode.Y2024.Day03, as: Solution

  test "Year 2024, Day 3 run/1" do
    assert Solution.run() == {190_604_937, 82_857_512}
  end
end
