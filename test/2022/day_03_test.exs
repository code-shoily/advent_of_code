defmodule AdventOfCode.Y2022.Day03Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2203

  alias AdventOfCode.Y2022.Day03, as: Solution

  test "Year 2022, Day 3 run/1" do
    assert Solution.run() == {8233, 2821}
  end
end
