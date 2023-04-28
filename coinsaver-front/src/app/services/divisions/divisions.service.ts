import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, catchError, throwError } from 'rxjs';
import { DivisionResponseDto } from 'src/app/dtos/transactions/response/division.response.dto';
import { TransactionCategoryTypeEnum } from 'src/app/enums/transaction-category-type.enum';
import { environment } from 'src/app/environments/environments';

@Injectable({
  providedIn: 'root'
})
export class DivisionsService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  baseUrl = environment.api.hostBackend;
  divisionControllerUrl = environment.api.divisionsBackendEndpoints;

  getDivisionByCategoryType(categoryType: TransactionCategoryTypeEnum): Observable<DivisionResponseDto[]> {
    return this.httpClient.get<DivisionResponseDto[]>(
      `${this.baseUrl +
      this.divisionControllerUrl +
      environment.api.divisionsBackendEndpoints.getDivisionByCategoryType
      }/${categoryType}`
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }

  getDivisionsById(divisionId: number): Observable<DivisionResponseDto> {
    return this.httpClient.get<DivisionResponseDto>(
      `${this.baseUrl +
      this.divisionControllerUrl +
      environment.api.divisionsBackendEndpoints.getDivisionById
      }/${divisionId}`
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }
}
