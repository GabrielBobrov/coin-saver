import { Component, OnInit } from '@angular/core';
import { PrimeIcons } from 'primeng/api';
import { MonthlyTransactionResponseDto } from 'src/app/dtos/transactions/response/monthly-transactions.response.dto';
import { MonthlyResponseDto } from 'src/app/dtos/transactions/response/monthly.response.dto';
import { TransactionsService } from 'src/app/services/transactions/transactions.service';

@Component({
  selector: 'app-table-geral',
  templateUrl: './table-geral.component.html',
  styleUrls: ['./table-geral.component.css']
})
export class TableGeralComponent implements OnInit {

  monthlyResponseDto: MonthlyResponseDto | undefined;
  monthlyTransactionsResponseDtoList: MonthlyTransactionResponseDto[] = [];

  events1: any[] = [];

  constructor(
    private transactionsService: TransactionsService,
    ) {}

  ngOnInit() {
      this.getAllTransactions();

      this.events1 = [
        {
          status: "Ordered",
          date: "15/10/2020 10:30",
          icon: PrimeIcons.SHOPPING_CART,
          color: "#9C27B0",
          image: "game-controller.jpg"
        },
        {
          status: "Processing",
          date: "15/10/2020 14:00",
          icon: PrimeIcons.COG,
          color: "#673AB7"
        },
        {
          status: "Shipped",
          date: "15/10/2020 16:15",
          icon: PrimeIcons.ENVELOPE,
          color: "#FF9800"
        },
        {
          status: "Delivered",
          date: "16/10/2020 10:00",
          icon: PrimeIcons.CHECK,
          color: "#607D8B"
        }
      ];
  }

  getAllTransactions() {
    this.transactionsService.getAllTransactions()
      .subscribe((res) => {

        console.log('all', res)
      });
  }

}
