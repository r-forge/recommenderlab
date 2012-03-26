/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.mahout.graph.linkanalysis;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;


import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.util.ToolRunner;
import org.apache.mahout.common.AbstractJob;
import org.apache.mahout.graph.model.Edge;
import org.apache.mahout.graph.model.Vertex;
import org.apache.mahout.math.DenseVector;
import org.apache.mahout.math.Vector;

/**
 * <p>Distributed computation of the PageRank a directed graph</p>
 *
 * <p>The input files need to be a {@link org.apache.hadoop.io.SequenceFile} with {@link Edge}s as keys and
 * any Writable as values and another {@link org.apache.hadoop.io.SequenceFile} with {@link IntWritable}s as keys and {@link Vertex} as
 * values, as produced by {@link org.apache.mahout.graph.preprocessing.GraphUtils#indexVertices(Configuration, Path, Path)}</p>
 *
 * <p>This job outputs text files with a vertex id and its pagerank per line.</p>
  *
 * <p>Command line arguments specific to this class are:</p>
 *
 * <ol>
 * <li>--output=(path): output path</li>
 * <li>--vertexIndex=(path): Directory containing vertex index as created by GraphUtils.indexVertices()</li>
 * <li>--edges=(path): Directory containing edges of the graph</li>
 * <li>--numVertices=(Integer): number of vertices in the graph</li>
 * <li>--numIterations=(Integer): number of numIterations, default: 5</li>
 * <li>--stayingProbability=(Double): probability not to teleport to a random vertex, default: 0.8</li>
 * </ol>
 *
 * <p>General command line options are documented in {@link AbstractJob}.</p>
 *
 * <p>Note that because of how Hadoop parses arguments, all "-D" arguments must appear before all other arguments.</p>
 */
public class PageRankJob extends RandomWalk {

  public static void main(String[] args) throws Exception {
    ToolRunner.run(new PageRankJob(), args);
  }

  @Override
  protected Vector createDampingVector(int numVertices, double stayingProbability) {
    return new DenseVector(numVertices).assign((1.0 - stayingProbability) / numVertices);
  }
}
