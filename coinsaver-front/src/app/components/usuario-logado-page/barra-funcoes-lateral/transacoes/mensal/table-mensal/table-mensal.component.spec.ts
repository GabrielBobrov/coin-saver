import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TableMensalComponent } from './table-mensal.component';

describe('TableMensalComponent', () => {
  let component: TableMensalComponent;
  let fixture: ComponentFixture<TableMensalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TableMensalComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(TableMensalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
