import { TransactionsService } from './../../services/transactions/transactions.service';
import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-usuario-logado-page',
  templateUrl: './usuario-logado-page.component.html',
  styleUrls: ['./usuario-logado-page.component.css']
})
export class UsuarioLogadoPageComponent implements OnInit {

  constructor(
    private transactionsService: TransactionsService,
  ) {}

  ngOnInit(): void {
    this.getAllTransactions();
  }

  // apenas para teste - OK
  getAllTransactions() {
    this.transactionsService.getTransaction()
      .subscribe((res) => {

        console.log(res)
      });
  }

}
