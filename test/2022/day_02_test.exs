defmodule AdventOfCode.Y2022.Day02Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2202

  alias AdventOfCode.Y2022.Day02, as: Solution

  test "Year 2022, Day 2 run/1" do
    assert Solution.run() == {12_645, 11_756}
  end
end
