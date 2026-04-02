defmodule AdventOfCode.Y2023.Day21Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2321

  alias AdventOfCode.Y2023.Day21, as: Solution

  test "Year 2023, Day 21 run/1" do
    assert Solution.run() == {3770, 628_206_330_073_385}
  end
end
