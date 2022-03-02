package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;


/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver {

    private final List<ForkJoinSolver> solvers = new ArrayList<>();
    private static final Set<Integer> visitedNodes = new ConcurrentSkipListSet<>();


    private int player;
    private static boolean goalFound = false;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        //this.visited = new ConcurrentSkipListSet<>();
        this.player = maze.newPlayer(start);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    private ForkJoinSolver(Maze maze, int newPlayer, int startNode, Set<Integer> visitedNodes){
        super(maze);
        this.player = newPlayer;
        this.start = startNode;
        //this.visited = visitedNodes;
    }



    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {

        // one player active on the maze at start
        //int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);
        visitedNodes.add(start);
        // as long as not all nodes have been processed
        while (!frontier.empty() && !goalFound) {

            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                goalFound = true;
                return pathFromTo(start, current);
            }
            // if current node has not been visited yet

            //if (visitedNodes.add(current)) {
                // move player to current node

                // mark node as visited
                ;
                // for every node nb adjacent to current
                //Set<Integer> nbs = maze.neighbors(current);

                //ArrayList<Integer> nbs = new ArrayList<>();

                /*
                for (int nb : maze.neighbors(current)) {
                    if(!visitedNodes.contains(nb)){
                        nbs.add(nb);
                    }
                }

                 */

                List<Integer> nbs = new ArrayList<>(maze.neighbors(current));
                boolean skipped = false;
                maze.move(player, current);
                for(int i = 0; i < nbs.size(); i++){
                    int nb = nbs.get(i);
                    if(visitedNodes.add(nb)){

                        if(!skipped){
                            frontier.push(nb);

                            skipped = true;
                        }
                        else {
                            int newPlayer = maze.newPlayer(nb);
                            ForkJoinSolver solver = new ForkJoinSolver(maze, newPlayer, nb, visitedNodes);
                            solvers.add(solver);
                            solver.fork();
                        }
                        predecessor.put(nb, current);

                    }
                }


            //}


        }

        for (ForkJoinSolver solver : solvers){
            List<Integer> path = solver.join();
            if (path != null) {
                int solverStartPos = path.remove(0);
                List<Integer> currentPath = pathFromTo(start, solverStartPos);

                currentPath.addAll(path);
                return currentPath;
            }
        }
        // all nodes explored, no goal found
        return null;
    }


}
