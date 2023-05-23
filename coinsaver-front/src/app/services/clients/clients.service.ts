import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, map, throwError } from 'rxjs';
import { environment } from 'src/app/environments/environments';

@Injectable({
  providedIn: 'root'
})
export class ClientsService {

  constructor(
    private httpClient: HttpClient,
  ) { }

  baseUrl = environment.api.hostBackend;
  clientsControllerUrl = environment.api.clientsControllerBackend;

  // recoverPassword(email?: string) {
  //   const url = this.baseUrl + this.clientsControllerUrl + environment.api.clientsBackendEndpoints.recoverPassword
  //   const body = JSON.stringify(email);
  //   const headers = { 'Content-Type': 'application/json' };

  //   return this.httpClient.post(url, body, { headers }).pipe(
  //     map(result => {
  //       return result;
  //     })
  //   );
  // }

  recoverPassword(email?: string) {
    const url = this.baseUrl + this.clientsControllerUrl + environment.api.clientsBackendEndpoints.recoverPassword
    const headers = { 'Content-Type': 'application/json' };

    return this.httpClient.post(
      (`${url}?email=${email}`), headers
    )
      .pipe(
        catchError((erroResponse) => {
          return throwError(erroResponse);
        })
      );
  }
}
