defmodule AdventOfCode.Y2015.Day7Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y157

  @raw_input "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i\n"

  alias AdventOfCode.Y2015.Day7, as: Solution
  alias Solution.Stack

  describe "Stack Test" do
    setup do
      %{stack: Stack.new()}
    end

    test "creates an empty stack" do
      assert %Stack{data: data} = Stack.new()
      assert data == {[], []}
    end

    test "pushes stacks properly", %{stack: stack} do
      stack = Stack.push(stack, 10)
      stack = Stack.push(stack, 20)
      stack = Stack.push(stack, 30)

      assert %Stack{data: {[30, 20], [10]}} == stack
    end

    test "pops the last element from stack", %{stack: stack} do
      stack = Stack.push(stack, 10)
      stack = Stack.push(stack, 20)
      stack = Stack.push(stack, 30)

      assert {30, stack} = Stack.pop(stack)
      assert {20, stack} = Stack.pop(stack)
      assert {10, _} = Stack.pop(stack)
    end

    test "popping empty stack returns nothing", %{stack: stack} do
      assert Stack.pop(stack) == {:empty, {[], []}}
    end

    test "check for empty stack", %{stack: stack} do
      assert Stack.empty?(stack)
      stack = Stack.push(stack, 1000)
      refute Stack.empty?(stack)
    end
  end

  describe "process/1" do
    test "splits the data into command vector" do
      expected = %{
        x: 123,
        y: 456,
        d: {:and, :x, :y},
        e: {:or, :x, :y},
        f: {:lshift, :x, 2},
        g: {:rshift, :y, 2},
        h: {:not, :x},
        i: {:not, :y}
      }

      assert Solution.process(@raw_input) == expected
    end
  end

  # @tag :current
  # test "Year 2015, Day 7, Part 1" do
  #   assert Solution.run_1(@raw_input, :i) == nil
  # end

  # test "Year 2015, Day 7, Part 2" do
  #   assert Solution.run_2() == nil
  # end
end
