import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TableGeralComponent } from './table-geral.component';

describe('TableGeralComponent', () => {
  let component: TableGeralComponent;
  let fixture: ComponentFixture<TableGeralComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TableGeralComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(TableGeralComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
