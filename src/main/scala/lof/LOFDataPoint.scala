package lof

import distance.Distance
import models.Identifiable
import org.apache.commons.math3.special.Erf.erf

import scala.concurrent.{ExecutionContext, Future}
import scala.math.{BigDecimal, max, sqrt}

trait LOFDataPoint[T<:LOFDataPoint[T]] extends Distance[T] with Identifiable{self: T=>

  /**
    * Get the local reachability distance of `p`
    * @return
    */
  def getLRD(k: Int)(implicit dataSource: LOFDataSource[T], ec: ExecutionContext): Future[BigDecimal] = {

    //  First, get the kNNs of `p`:
    val kNN = dataSource.getKNN(self, k)

    kNN.flatMap{case (_, neighborhood, _)=>
      //  Then, get the support points:
      Future.sequence(
        neighborhood.map{case (p, _)=> dataSource.getKNN(p, k)}
      ).map { n =>
        1 / n.map { case (neighbor: T, knn, _) =>
          //    Get the k-distance for each point:
          val kDist = knn.maxBy { case (_, d) => d }._2

          //    Use it to get the reachability distance:
          kDist.max(distance(neighbor))
        }.foldLeft(BigDecimal.valueOf(0))(_ + _) / neighborhood.size
      }
    }
  }

  /**
    * Obtain the Local Outlier Factor of `p`
    * @return
    */
  def getLOF(k: Int)(implicit dataSource: LOFDataSource[T], ec: ExecutionContext): Future[BigDecimal] = {
    for{
      (_, neighborhood, _) <- dataSource.getKNN(self, k)
      lrdP <- getLRD(k)
      lrdQ <- Future.sequence(
        neighborhood.map{case (q, _)=>
          q.getLRD(k)
        }
      )
    } yield {
      lrdQ.map(_ / lrdP).sum/neighborhood.size
    }

  }

  /**
    * Gets the standard distance of `p` with respect to its neighborhood
    * @return
    */
  def sigmaDist(k: Int)(implicit dataSource: LOFDataSource[T], ec: ExecutionContext): Future[BigDecimal] = {
    dataSource.getKNN(self, k).map{case (p, neighborhood, k)=>
      neighborhood.foldLeft(BigDecimal.valueOf(0)){case (acc, v)=>
        acc + v._2.pow(2)
      }/neighborhood.size
    }
  }

  /**
    * Gets the probability-distance of `p` for a significance level λ
    * @param λ
    * @return
    */
  def pDist(k: Int, λ: BigDecimal)(implicit dataSource: LOFDataSource[T], ec: ExecutionContext): Future[BigDecimal] = {
    sigmaDist(k).map(λ * _)
  }

  /**
    * Get the Probability Local Outlier Factor (PLOF) of `p`
    * @param λ
    * @return
    */
  def getPLOF(k: Int, λ: BigDecimal)(implicit dataSource: LOFDataSource[T], ec: ExecutionContext): Future[BigDecimal] = {
    for{
      (_, neighborhood, _) <- dataSource.getKNN(self, k)
      pDistP <- pDist(k, λ)
      pDistQ <- Future.sequence(
        neighborhood.map{case (q, _)=>
          q.pDist(k, λ)
        }
      ).map {pdistsQ=>
        pdistsQ.sum / pdistsQ.size
      }
    } yield {
      (pDistP/pDistQ) - 1
    }
  }

  /**
    * Gets the Local Outlier Probability for point `p` and significance level λ
    * @param λ
    * @return
    */
  def getLoOP(k: Int, λ: BigDecimal)(implicit dataSource: LOFDataSource[T], ec: ExecutionContext): Future[BigDecimal] = {
    getPLOF(k, λ).flatMap{plof=>
      dataSource.getNPLOF(k, self, λ).map{nPlof=>
        max(0, erf((plof/(nPlof*sqrt(BigDecimal.valueOf(2).toDouble))).toDouble))
      }
    }
  }

}
